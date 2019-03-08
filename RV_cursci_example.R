
#--------------------------------------------------------#
# The following script containts the code underlying the
# Curate Science example case in the Replication Value 
# manuscript.
#
# The replication value used for this example:
# Yearly citation rate divided by adjusted sample size
#--------------------------------------------------------#


# Load dataset and relevant packages

library(rcrossref)
library(jsonlite)
library(tidyverse)
library(googlesheets)
source("../Meta-Data/analysis/MetaData_functions.R")
source("../2018_OSC_Replication_Value/RV_functions.R")
curate_science <- read.csv("https://raw.githubusercontent.com/eplebel/science-commons/master/CS.rep.table.csv", na.strings = "", stringsAsFactors = FALSE)
cursci_orig_cr <- readRDS("curate_science_orig_study_crossref.Rds")



# Retrieve crossref metrics for original studies in the dataset (only needed once) ####

cursci_orig_cr <- cr_cn(dois = unique(curate_science$orig.study.article.DOI), "citeproc-json")  # Download DOI meta-data from crossref for each unique DOI
write_json(cursci_orig_cr, "curate_science_orig_study_crossref.json", pretty = TRUE)
saveRDS(cursci_orig_cr, "curate_science_orig_study_crossref.Rds")

# Dataset set-up ####

## extract datapoints from crossref data to be merged with curate_science
years <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`issued`$`date-parts`[1]), NA, x$`issued`$`date-parts`[1])))
citations <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`is-referenced-by-count`[1]), NA, x$`is-referenced-by-count`[1])))
doi <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`DOI`[1]), NA, x$`DOI`[1]))) 
cr.df <- data.frame(orig.publ.year = years, orig.citations = citations, orig.study.article.DOI = doi)  

curate_science$orig.study.article.DOI <- tolower(curate_science$orig.study.article.DOI)  # set DOI characters in curate_science to lower case to match DOI formatting in cr.df
curate_science <- merge(curate_science, cr.df, by = "orig.study.article.DOI")  # Some DOIs do not match, leading to a smaller dataset.



# Calculate replication value for original studies 

adjusted_n_orig <- sapply(1:nrow(curate_science), function(i) {
  
  x <- curate_science$orig.N[i]  # sample size
  f <- 1  # assume 1 factor
  lvl <- 2  # assume 2 levels
  
  if (grepl(";", curate_science$IVs[i]) & !grepl("Hughes et al", curate_science$IVs[i])) {  # Find all studies with more than one IV, but ignore the one case where ";" is not used to separate IVs 
    f <- length(unlist(strsplit(curate_science$IVs[i], ";")))  # Count factors in interaction
    lvl <- 2  # Assume for simplicity that all factors have 2 levels, which in this dataset is often but not always correct
    x <- x / (2^(f-1)) / (f*lvl) * 2  # Applies the formula for interaction conversion in SM1, and multiplies the result by two to get the total sample size for a 2 group effect. 
  } 
  
  if (grepl("within|^dependent|RM|paired|repeated", curate_science$design[i]) | curate_science$orig.ES.type[i] %in% "dz") {  # Find within-subject designs based on the effects size calculated (dz always used to refer to within-subject effect. PD was only used for precognition replication, also a within-subject design)
    x <- x * (f*lvl)/(1-0)  # Applies the formula for within-subject design conversin in SM1. The calculation assumes two groups and no correlation between the groups.
  }
  return(x)
})

curate_science$orig.N.adj <- unlist(adjusted_n_orig)
curate_science$orig.RV <- curate_science$orig.citations / (as.numeric(format(Sys.Date(), "%Y")) - curate_science$orig.publ.year) * (1/curate_science$orig.N.adj-3)



# Calculate combined adjusted n of replications

adjusted_n_rep <- sapply(1:nrow(curate_science), function(i) {
  x <- curate_science$rep.N[i]
  f <- 1
  lvl <- 2
  if (grepl(";", curate_science$IVs[i]) & !grepl("Hughes et al", curate_science$IVs[i])) {  # Find all studies with more than one IV, but ignore the one case where ";" is not used to separate IVs 
    f <- length(unlist(strsplit(curate_science$IVs[i], ";")))  # Count factors in interaction
    lvl <- 2  # Assume for simplicity that all factors have 2 levels, which in this dataset is often but not always correct
    x <- x / (2^(f-1)) / (f*lvl) * 2  # Applies the formula for interaction conversion in SM1, and multiplies the result by two to get the total sample size for a 2 group effect. 
  } 
  if (grepl("within|^dependent|RM|paired|repeated", curate_science$design[i]) | curate_science$rep.ES.type[i] %in% "dz") {  # Find within-subject designs based on the effects size calculated (dz always used to refer to within-subject effect. PD was only used for precognition replication, also a within-subject design)
    x <- x * (f*lvl)/(1-0)  # Applies the formula for within-subject design conversin in SM1. The calculation assumes two groups and no correlation between the groups.
  }
  return(x)
})

curate_science$rep.N.adj <- adjusted_n_rep

sum_n_orig <- aggregate(curate_science$orig.N.adj, by=list(orig.study.number=curate_science$orig.study.number), FUN=mean)
sum_n_rep <- aggregate(curate_science$rep.N.adj, by=list(orig.study.number=curate_science$orig.study.number), FUN=sum)
sum_n_rep$sum.N <- as.numeric(sum_n_rep$x + sum_n_orig$x)
sum_n_rep$x <- NULL

curate_science <- merge(x = curate_science, y = sum_n_rep, by = "orig.study.number", all = TRUE)



# Calculate for Fisher Z SE
curate_science$orig.RV <- curate_science$orig.citations / (as.numeric(format(Sys.Date(), "%Y")) - curate_science$orig.publ.year) * sqrt((1 / (curate_science$orig.N.adj - 3)))
curate_science$rep.RV <- curate_science$orig.citations / (as.numeric(format(Sys.Date(), "%Y")) - curate_science$orig.publ.year) * sqrt((1 / (curate_science$orig.N.adj + curate_science$rep.N.adj - 3)))  # Calculate RV of original plus replication for every replication
curate_science$sum.RV <- curate_science$orig.citations / (as.numeric(format(Sys.Date(), "%Y")) - curate_science$orig.publ.year) * sqrt((1 / (curate_science$sum.N - 3)))










# Aggregate results 

RVdata <- aggregate(curate_science$orig.N.adj, by=list(orig.DOI=curate_science$orig.study.article.DOI, orig.study.number=curate_science$orig.study.number, orig.RV=curate_science$orig.RV, sum.RV=curate_science$sum.RV, y.cit=(curate_science$orig.citations/(2018-curate_science$orig.publ.year))), FUN=mean)
names(RVdata)[length(names(RVdata))] <- "orig.N"
RVdata <- RVdata[order(-RVdata$orig.RV),]
RVdata$order <- 1:nrow(RVdata)
names(RVdata)[2:3] <- c("original", "total")

RVdata <- gather(data = RVdata, key = "stage", value = "RV", original:total, factor_key = TRUE)
highest4 <- RVdata$orig.study.number[RVdata$RV %in% tail(sort(RVdata$RV[RVdata$stage == "original"]), 4)]  # Find 4 highest total replication values
RVdata$highRV <- ifelse((RVdata$orig.study.number %in% highest4 | RVdata$orig.study.number == "Stroop (1935) Study 2") & RVdata$stage == "original", RVdata$RV, NA)  # Mark variables to be labeled in the plot



# Derive relevant statistics and plots

## Median replication value of original findings
med.RVorig <- median(RVdata$RV[RVdata$stage == "original"])

## Median replication value, replications included
med.RVtot <- median(RVdata$RV[RVdata$stage == "total"])

## Mean percentage decrease in repication value for every replication
RVrepdec <- (curate_science$orig.RV - curate_science$rep.RV) / curate_science$orig.RV * 100
m.RVrepdec <- mean(RVrepdec, na.rm = TRUE)

## Mean percentage decrease in repication value from original to total
RVsumdec <- (RVdata$RV[RVdata$stage == "original"] - RVdata$RV[RVdata$stage == "total"]) / RVdata$RV[RVdata$stage == "original"] * 100
m.RVsumdec <- mean(RVsumdec, na.rm = TRUE)

## Plots

### Scatterplot of original and summary replication values, sorted by size
ggplot(data = RVdata, aes(x = order, y = RV)) + 
  geom_point(aes(col = stage), size = 3) + 
  geom_text(aes(y = RVdata$highRV, label = orig.study.number), na.rm = TRUE, size = 5, hjust = -.02, vjust = .0, check_overlap = FALSE) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  scale_color_manual(values = c("black", "#3f75cc"))+
  labs(x = "Original study, sorted by original replication value", y = "Replication value", title = "Curate Science Replications")

### histogram of replication value decrease for every replication
hist(RVrepdec, breaks = 30, density = 20)

### histogram of sum replication value decrease
hist(RVsumdec, breaks = 50, density = 30)


# Extract sample of high and low replication values for qualitative inspection

## Extract highest and lowest ranked studies, including title, authors, year, abstract, citation info, altmetrics, sample size, and effect size.

## Correlate replication value of original with number of replication studies



