
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
library(ggplot2)
library(googlesheets)
source("../Meta-Data/MetaData_functions.R")
source("../2018_OSC_Replication_Value/RV_functions.R")
curate_science <- read.csv("https://raw.githubusercontent.com/eplebel/science-commons/master/CS.rep.table.csv", na.strings = "")




# Retrieve crossref metrics for original studies in the dataset

cursci_orig_cr <- rcrossref::cr_cn(dois = curate_science$orig.study.article.DOI, "citeproc-json")  # Download DOI meta-data from crossref 


curate_science <- curate_science[order(curate_science$orig.study.number),]  # Order data to match crossref meta-data order (first author, alphabetically decending)
curate_science$IVs <- as.character(curate_science$IVs)
curate_science$orig.publ.year <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`issued`$`date-parts`[1]), NA, x$`issued`$`date-parts`[1])))  # grab publication year from crossref meta-data
curate_science$orig.citations <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`is-referenced-by-count`[1]), NA, x$`is-referenced-by-count`[1])))  # grab citation count from crossref meta-data



# Calculate replication value for original studies 

adjusted_n_orig <- sapply(1:nrow(curate_science), function(i) {
  x <- curate_science$orig.N[i]
  f <- 1
  lvl <- 2
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

curate_science$orig.RV <- curate_science$orig.citations / (as.numeric(format(Sys.Date(), "%Y")) - curate_science$orig.publ.year) / curate_science$orig.N.adj





# Calculate updated replication value based on the combined adjusted n of replications

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

curate_science$sum.RV <- curate_science$orig.citations / (as.numeric(format(Sys.Date(), "%Y")) - curate_science$orig.publ.year) / curate_science$sum.N



# Plot replication values before and after aggregation
RVdata <- aggregate(curate_science$orig.N.adj, by=list(orig.study.number=curate_science$orig.study.number, orig.RV=curate_science$orig.RV, sum.RV=curate_science$sum.RV), FUN=mean)
RVdata <- RVdata[order(-RVdata$orig.RV),]
RVdata$highRV <- ifelse(RVdata$orig.RV > 1 | RVdata$orig.study.number == "Stroop (1935) Study 2", RVdata$orig.RV, NA)

ggplot(data = RVdata, aes(x = 1:nrow(RVdata), y = orig.RV)) + 
  geom_point() + 
  geom_point(aes(y = sum.RV), col = "blue") + 
  geom_text(aes(y = RVdata$highRV, label = orig.study.number), na.rm = TRUE, hjust = -.02, vjust = .0) +
  scale_colour_manual(name="RV",values=c("Original"="black")) +
  theme_classic() +
  labs(x = "Original study, sorted by original replication value", y = "Replication value")
