
## DOI metadata extraction
# This script file extracts bibliometric metadata based on DOI signatures for the metadata project.

#### load functions ####

library(googlesheets)
library(rcrossref)
library(rAltmetric)
library(rscopus)
library(foreach)

#### import gsheet ####

pbul.ginfo <- gs_title("complete_records_psychological_bulletin_1_1000")

gs_ws_ls(pbul.ginfo)

pbul.gsheet <- gs_read(ss = pbul.ginfo, ws = "Blad2")  # Download gsheet from Google sheets
pbul.gsheet <- as.data.frame(pbul.gsheet)  # Convert sheet to normal dataframe
pbul.gsheet <- pbul.gsheet[-1,]  # Remove variable definition row


#### test metadata linking ####

set_api_key('9ac91fdbf147316eeb53a16681ddc95f')
ma.index <- 16
doi <- pbul.gsheet[ma.index,"DI"]

## Extract reference list from a meta analysis

# Get scopus info for a meta-analysis doi
alist <- abstract_retrieval(doi, identifier = "doi")  # Get SCOPUS metadata for a meta analysis DOI (large list of information)


## Link meta analysis references and DOIs with appropriate rows in meta analysis table files
# Peder at work:
table.dir <- "C:/Users/20176239/Dropbox/jobb/PhD/Projects/Meta-Data/EXCEL_FILES/"  # Set the directory where meta analysis table files can be found

# Peder at home
table.dir <- "D:/Dropbox/jobb/PhD/projects/Meta-Data/EXCEL_FILES/2017/"  # Set the directory where meta analysis table files can be found


table.file <- list.files(path = table.dir, 
                         pattern = paste(pbul.gsheet$VL[ma.index], "_", pbul.gsheet$IS[ma.index], "_", pbul.gsheet$BP[ma.index], ".*csv", sep = ""), 
                         recursive = TRUE)  # Locate relevant table files, based on the row-index of the doi in the meta-analysis gsheet.


# Set up the ma.files list, aquire the necessary Scopus data, and list each scopus file together with the corresponding meta analysis DOI and table file locations
ma.files <- sapply(1:10, 
                      function(i) {
                        ma.index <- i
                        doi <- pbul.gsheet[ma.index,"DI"]
                        if (!is.na(doi)) { 
                          table.file <- list.files(path = table.dir, 
                                                   pattern = paste(pbul.gsheet$VL[ma.index], "_", pbul.gsheet$IS[ma.index], "_", pbul.gsheet$BP[ma.index], ".*csv", sep = ""), 
                                                   recursive = TRUE)  # Locate relevant table files, based on the row-index of the doi in the meta-analysis gsheet.
                        }
                        if (length(table.file > 0)) { 
                          scopus.list <- abstract_retrieval(doi, identifier = "doi") 
                          return(list(doi=doi, table.files=table.file, scopus.list=scopus.list))
                        }
                      }
)

save(ma.files, file = "ma.files.RData")






# Extract DOIs for a given meta analysis table
load("ma.files")


foreach(i = ma[["table.files"]]) %do% {  # Setting up foreach loop instead of for, to be able to do parallell processing later on.
  
  ma.table <- read.csv(paste(table.dir, table.files, sep = ""), na.strings = c(""))  # Load the relevant meta analysis table
  ma.table$x_doi <- NA  # Set up DOI column
  matched.refs.doi.missing <- NULL
  

    table.info <- sapply(ma.table$x_study, 
                         function(i) {
                           x <- as.character(i)
                           x <- strsplit(x, "[, &()]")
                           x <- as.matrix(unlist(x, recursive = TRUE))
                           as.matrix(x[!x %in% c("", "et", "al.")])
                         }
    )
    
    scopus.info <- sapply(alist[["content"]][["abstracts-retrieval-response"]][["item"]][["bibrecord"]][["tail"]][["bibliography"]][["reference"]], 
                          function(i) {
                            y <- as.matrix(sapply(i$`ref-info`$`ref-authors`$author, `[[`, 4))
                            y <- as.matrix(y)
                            y[nrow(y)+1] <- try(as.matrix(i$`ref-info`$`ref-publicationyear`))
                            as.matrix(as.character(y))
                          }
    )
    
    
    matches <- data.frame()
    for (ii in seq_along(table.info)) {
      t.ref <- table.info[[ii]]
      for (iii in seq_along(scopus.info)) {
        s.ref <- scopus.info[[iii]]
        if (nrow(t.ref) <= nrow(s.ref) & nrow(t.ref) > 1) {  # scopus info must have at least as many authors as table info, and table info must have at least one author and year info
          s.ref <- s.ref[c(2:nrow(t.ref)-1, nrow(s.ref)),]
          s.ref <- matrix(s.ref)
          t.ref <- matrix(t.ref) 
          if (all(s.ref == t.ref)) {
            matches <- rbind(matches, c(ii, iii))
          }
        }
      }
    }
    
    names(matches) <- c("t.index", "s.index")  # give matches columns sensible names
    
    dbl.in <- matches$t.index[duplicated(matches$t.index)]  # Identify table rows that matched more than one scopus reference.
    
    
    # Extract DOIs from scopus reference list
    refs <- alist[["content"]][["abstracts-retrieval-response"]][["item"]][["bibrecord"]][["tail"]][["bibliography"]][["reference"]]  # Get references
    dois <- sapply(refs, function(i) x <- i[["ref-info"]][["ref-website"]][["ce:e-address"]][["$"]] )  # Extract DOIs from references
    nulls <- grep("NULL", dois)  # Identify references with missing DOI
    dois[nulls] = NA  # Insert replace NULL with NA to preserve missing values during unlist
    dois <- as.matrix(unlist(dois))
    
    # For ma.table rows matching exactly 1 reference from scopus, insert DOI from that reference
    for (ii in 1:nrow(ma.table)) {  # Loop through every row in ma.table
      if (!ii %in% dbl.in & ii %in% matches$t.index) {  # Exclude ma.table rows matching more than 1 reference
        match.ref <- matches$s.index[matches$t.index == ii]  # Store index of match in reference list
        if (!is.na(dois[match.ref])) {  # If there is  DOI for the reference...
          ma.table$x_doi[ii] <- dois[match.ref]  # Call the doi of the matching reference
        } else {  # Search crossref for DOI if none is available in the rscopus data
          author.1 <- refs[[match.ref]][["ref-info"]][["ref-authors"]][["author"]][[1]][["ce:surname"]]
          year <- refs[[match.ref]][["ref-info"]][["ref-publicationyear"]][["@first"]]
          titl <- refs[[match.ref]][["ref-info"]][["ref-title"]][["ref-titletext"]]
          titl <- gsub("\\[|\\]", "", titl)  # Remove any square brackets from title, as this will mess with the grep function below
          print(paste("Searching Crossref for info on", author.1, year, ", scopus ref # =", match.ref))
          search.works <- cr_works(query = paste("author:", author.1,
                                                 " year:", year,
                                                 " title:", titl, sep = ""))  # Search crossref works for DOIs
          
          # Find the DOI matching the refereence listed in the rscopus table
          search.dois <- unlist(cr_cn(search.works[["data"]][["DOI"]][1:5]))  # Search crossref for publications matching the first 5 DOIs found in search
          doi.match <- grep(paste(".*", author.1, ".*", year, ".*", titl, ".*", sep = ""), search.dois, ignore.case = TRUE)  # Find publication matching the first-author, year, and title of the reference from rscopus
          if (length(doi.match) == 0) {  # If no match can be found, print message
            print(paste("No DOI could be found in crossref for search: ", ".*", author.1, ".*", year, ".*", titl, ".*", sep = ""))
            ma.table$x_doi[ii] <- NA
            matched.refs.doi.missing <- c(matched.refs.doi.missing, match.ref)
          } else if (length(doi.match) > 2) {  # If more than one match is found, grab none, and print message
            print(paste("Info for more than one DOI in crossref matches search: ", ".*", author.1, ".*", year, ".*", titl, ".*", sep = ""))
          } else if (length(doi.match) == 1) {  # If only one match is found, print matching DOI to table
            print(paste("Found and printed 1 match for search: ", ".*", author.1, ".*", year, ".*", titl, ".*", sep = ""))
            ma.table[ii, "x_doi"] <- search.works[["data"]][["DOI"]][[doi.match]]
          }
        } 
      }
    } 
    
    # Create list of double matches for manual DOI retreval
    dbl.match <- matches[matches$t.index %in% dbl.in,]  # Get rows in "matches" with multiple matches
    dbl.match$s.article <- sapply(refs[dbl.match$s.index], function(i) x <- i[["ref-fulltext"]] )  # add reference of all matches in scopus
    names(dbl.match) <- c("table.row", "ma.reference.#", "ma.reference")  # Give sensible names
    assign(paste0("double_matched_refs_", doi), dbl.match)  # Assign to a variable unique to each meta analysis doi
    
    

  write.csv(x = ma.table, file = paste(table.dir, i, sep = ""))  # Save edited table back to csv
}




