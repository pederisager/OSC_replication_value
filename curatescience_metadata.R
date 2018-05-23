
#### Load functions and data ####

library(rcrossref)
library(rAltmetric)

#### Get rcrossref data ####

cursci <- read.csv(file = "curatescience.csv", sep = ",", header = TRUE, na.strings = c("", "NA"))

#### Add DOI-based data into the curate science dataset ####

cursci <- getCRcitations(table = cursci, doi.col = "orig.study.article.DOI")
cursci <- getAltmetrics(table = cursci, doi.col = "orig.study.article.DOI")




