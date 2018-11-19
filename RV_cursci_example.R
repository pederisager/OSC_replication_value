
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
library(tidyverse)
library(googlesheets)
source("../Meta-Data/MetaData_functions.R")
source("../2018_OSC_Replication_Value/RV_functions.R")
curate_science <- read.csv("https://raw.githubusercontent.com/eplebel/science-commons/master/CS.rep.table.csv", na.strings = "")




# Retrieve crossref metrics for original studies in the dataset (only needed once)

cursci_orig_cr <- rcrossref::cr_cn(dois = curate_science$orig.study.article.DOI, "citeproc-json")  # Download DOI meta-data from crossref 



# Dataset set-up

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

curate_science$rep.RV <- curate_science$orig.citations / (as.numeric(format(Sys.Date(), "%Y")) - curate_science$orig.publ.year) / (curate_science$orig.N.adj + curate_science$rep.N.adj)  # Calculate RV of original plus replication for every replication
curate_science$sum.RV <- curate_science$orig.citations / (as.numeric(format(Sys.Date(), "%Y")) - curate_science$orig.publ.year) / curate_science$sum.N



# Aggregate results for plotting purposes

RVdata <- aggregate(curate_science$orig.N.adj, by=list(orig.study.number=curate_science$orig.study.number, orig.RV=curate_science$orig.RV, sum.RV=curate_science$sum.RV, y.cit=(curate_science$orig.citations/(2018-curate_science$orig.publ.year))), FUN=mean)
RVdata <- RVdata[order(-RVdata$orig.RV),]
RVdata$order <- 1:nrow(RVdata)
names(RVdata)[2:3] <- c("original", "total")
RVdata <- gather(data = RVdata, key = "stage", value = "RV", original:total, factor_key = TRUE)

highest4 <- RVdata$orig.study.number[RVdata$RV %in% tail(sort(RVdata$RV[RVdata$stage == "total"]), 4)]  # Find 4 highest total replication values
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
  geom_point(aes(col = stage)) + 
  geom_text(aes(y = RVdata$highRV, label = orig.study.number), na.rm = TRUE, hjust = -.02, vjust = .0, check_overlap = FALSE) +
  theme_classic() +
  scale_color_manual(values = c("black", "#3f75cc"))+
  labs(x = "Original study, sorted by original replication value", y = "Replication value")

### histogram of replication value decrease for every replication
hist(RVrepdec, breaks = 30, density = 20)

### histogram of sum replication value decrease
hist(RVsumdec, breaks = 50, density = 30)
