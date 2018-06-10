
#### Load functions and data ####

library(rcrossref)
library(rAltmetric)

source("../Meta-Data/MetaData_functions.R")
source("../2018_OSC_Replication_Value/RV_functions.R")

#### Get rcrossref data ####

cursci <- read.csv(file = "curatescience.csv", sep = ",", header = TRUE, na.strings = c("", "NA"))

#### Add DOI-based data into the curate science dataset ####

cursci <- getCRcitations(table = cursci, doi.col = "orig.study.article.DOI")
cursci <- getAltmetrics(table = cursci, doi.col = "orig.study.article.DOI")
cursci$x_altscore <- as.numeric(cursci$x_altscore)






#### Calculate replication values for original studies ####


## rvCAN with citations weighted as twice as important as altmetrics

cursci$rvCAN <- sapply(1:nrow(cursci), function(x) {
  try(rvCAN(citations = cursci[[x,"x_crcited"]], altmetric = cursci[[x,"x_altscore"]], n = cursci[[x,"orig.N"]], weight = c(2,1,2)))
})

# Is the RV higher for original effects that are inconsistent with repications? 
mean(cursci[grep("- consistent", cursci$rep.outcome), "rvCAN"], na.rm = TRUE)
mean(cursci[grep("- inconsistent", cursci$rep.outcome), "rvCAN"], na.rm = TRUE)



## rvCAN using average citation and altmetrics score since year of publication

current.year <- as.numeric(format(Sys.Date(), "%Y"))
cursci$x_altscore_pyear <- cursci$x_altscore / (current.year - cursci$orig.study.pub.year)
cursci$x_crcited_pyear <- cursci$x_crcited / (current.year - cursci$orig.study.pub.year)

cursci$rvCAN_pyear <- sapply(1:nrow(cursci), function(x) {
  try(rvCAN(citations = cursci[[x,"x_crcited_pyear"]], altmetric = cursci[[x,"x_altscore_pyear"]], n = cursci[[x,"orig.N"]], weight = c(2,1,2)))
})

# Is the RV higher for original effects that are inconsistent with repications? 
mean(cursci[grep("- consistent", cursci$rep.outcome), "rvCAN_pyear"], na.rm = TRUE)
mean(cursci[grep("- inconsistent", cursci$rep.outcome), "rvCAN_pyear"], na.rm = TRUE)



## rvCAPwith citations weighted as twice as important as altmetrics

cursci$orig.ES.CI <- as.numeric(cursci$orig.ES.CI)

cursci$orig.ES.CI.r <- sapply(1:nrow(cursci), function(i) {  # Recalculate CIs to r values
  if (cursci$orig.ES.type[i] == "r") {
    
    cursci$orig.ES.CI[i]

      } else if (cursci$orig.ES.type[i] %in% c("g", "d")) {
    
    es <- cursci$orig.ES.CI[i]
    r <- es / sqrt(es^2 + 4)  # conversion formula from https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
    r
    
      } else {
    NA
  }
})


cursci$rvCAP <- sapply(1:nrow(cursci), function(x) {
  try(rvCAP(citations = cursci[[x,"x_crcited"]], altmetric = cursci[[x,"x_altscore"]], precision = cursci[[x,"orig.ES.CI.r"]], weight = c(1,0.5,1))) # 95% CI converted to r used as precision
})

## rvCAP using average citation and altmetrics score since year of publication
cursci$rvCAP_pyear <- sapply(1:nrow(cursci), function(x) {
  try(rvCAP(citations = cursci[[x,"x_crcited_pyear"]], altmetric = cursci[[x,"x_altscore_pyear"]], precision = cursci[[x,"orig.ES.CI.r"]], weight = c(1,0.5,1))) # 95% CI converted to r used as precision
})




