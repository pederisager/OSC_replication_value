
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
ma.index <- 18
doi <- pbul.gsheet[ma.index,"DI"]

## Extract reference list from a meta analysis

# Get scopus info for a meta-analysis doi
alist <- abstract_retrieval(doi, identifier = "doi")  # Get SCOPUS metadata for a meta analysis DOI (large list of information)


## Link meta analysis references and DOIs with appropriate rows in meta analysis table files
# Peder at work:
#table.dir <- "C:/Users/20176239/Dropbox/jobb/PhD/Projects/Meta-Data/EXCEL_FILES/"  # Set the directory where meta analysis table files can be found

# Peder at home
table.dir <- "D:/Dropbox/jobb/PhD/projects/Meta-Data/EXCEL_FILES/2017/"  # Set the directory where meta analysis table files can be found

table.file <- list.files(path = table.dir, 
                         pattern = paste(pbul.gsheet$VL[ma.index], "_", pbul.gsheet$IS[ma.index], "_", pbul.gsheet$BP[ma.index], ".*csv", sep = ""), 
                         recursive = TRUE)  # Locate relevant table files, based on the row-index of the doi in the meta-analysis gsheet.

foreach(i = table.file) %do% {  # Setting up foreach loop instead of for, to be able to do parallell processing later on.
  
  ma.table <- read.csv(paste(table.dir, table.file, sep = ""))  # Load the relevant meta analysis table
  ma.table$x_doi <- NA  # Set up DOI column
  nrefs <- length(alist[["content"]][["abstracts-retrieval-response"]][["item"]][["bibrecord"]][["tail"]][["bibliography"]][["reference"]])  # count number of references
  insertions <- NULL
  
  foreach (j = 1:nrefs) %do% {  # Loop through reference list, matching relevant references to x_study column in the meta analysis table and inserting the doi
    ref <- alist[["content"]][["abstracts-retrieval-response"]][["item"]][["bibrecord"]][["tail"]][["bibliography"]][["reference"]][[j]]  # grab reference info from rscopus list
    author.1 <- ref[["ref-info"]][["ref-authors"]][["author"]][[1]][["ce:surname"]]  # grab first author info
    year <- ref[["ref-info"]][["ref-publicationyear"]][["@first"]]  # grab publication year info
    titl <- ref[["ref-info"]][["ref-title"]][["ref-titletext"]]  # Grab publication title (needed for later DOI search)
    search.term <- paste("^", author.1, ".*", year, ".*", sep = "")  # make regular expression search term based on first author and year info
    if (!(is.null(author.1) | is.null(year) | is.null(titl))) {  # Only check references that have basic search info available
      index <- grep(search.term, ma.table$x_study)  # search meta analysis table for row indexes matching the search term
      
      if (length(unique(ma.table[index,"x_study"])) > 1) {  # Throw warning and abort DOI insertion if multiple unique table references match the search term
        authors <- NULL  # preallocate variable
        
        for (k in 1:length(ref[["ref-info"]][["ref-authors"]][["author"]])) {
          author.k <- ref[["ref-info"]][["ref-authors"]][["author"]][[k]][["ce:surname"]]  # Loop through rest of reference authors
          authors <- c(authors, author.k)
          match <- grep(paste("^", paste(authors, collapse = "[, &]+"),  ".*", year, ".*", sep = ""), ma.table$x_study)
          print(match)
          if (length(unique(ma.table[index,"x_study"])) == 1) {
            index <- match
            break
          }
        }
        
      }
      
      if (length(index) > 0) { 
        insertion <- cbind(c(index), paste("ref:", j))
        insertions <- rbind(insertions, insertion)
        if (!is.null(ref[["ref-info"]][["ref-website"]][["ce:e-address"]][[1]])) {  # Check whether DOI for reference exist
          ma.table[index, "x_doi"] <- ref[["ref-info"]][["ref-website"]][["ce:e-address"]][[1]][[1]]  # add DOI to meta analysis indexes identified above
        } else { # Search crossref for DOI if none is available in the rscopus data
          print(paste("Searching Crossref for info on", author.1, year, ", scopus ref # =", j))
          search.works <- cr_works(query = paste("author:", author.1,
                                                 " year:", year,
                                                 " title:", titl, sep = ""))  # Search crossref works for DOIs
          
          # Find the DOI matching the refereence listed in the rscopus table
          search.dois <- unlist(cr_cn(search.works[["data"]][["DOI"]]))  # Search crossref for publications matching DOIs found in search
          doi.match <- grep(paste(".*", author.1, ".*", year, ".*", titl, ".*", sep = ""), search.dois, ignore.case = TRUE)  # Find publication matching the first-author, year, and title of the reference from rscopus
          if (length(doi.match) == 0) {  # If no match can be found, print message
            print(paste("No DOI could be found in crossref for search: ", ".*", author.1, ".*", year, ".*", titl, ".*", sep = ""))
          } else if (length(doi.match) > 2) {  # If more than one match is found, grab none, and print message
            print(paste("Info for more than one DOI in crossref matches search: ", ".*", author.1, ".*", year, ".*", titl, ".*", sep = ""))
          } else if (length(doi.match) == 1) {  # If only one match is found, print matching DOI to table
            ma.table[index, "x_doi"] <- search.works[["data"]][["DOI"]][[doi.match]]
          }
        }
      }
    }
  }
  write.csv(x = ma.table, file = paste(table.dir, i, sep = ""))  # Save edited table back to csv
}



