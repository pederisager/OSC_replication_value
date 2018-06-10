
## 


#### Load packages, functions and data ####

source("RV_functions.R")

load("../Meta-Data/ma_tables.RData")

# Load data
columns <- c("x_study", "x_doi", "x_es", "x_es_type", "x_n", "x_crcited", "x_altscore")
master.df <- makeMasterdf(tables, columns)
master.df$x_altscore[master.df$x_altscore %in% "metrics_not_found"] <- NA
master.df$x_altscore <- as.numeric(master.df$x_altscore)
master.df$x_crcited <- as.numeric(master.df$x_crcited)

#### Calculate minimal replication value ####

can.df <- master.df[complete.cases(master.df[,c("x_n", "x_altscore", "x_crcited", "x_doi")]),]

can.df$altRV <- can.df$x_altscore / can.df$x_n

master.df$rvCAN <- rvCAN(citations = master.df$x_crcited, altmetric = master.df$x_altscore, n = master.df$x_n, weight = c(1, 0.5, 1))


#### Stats and values bases on replication value calculations ####

## Get unique replication URLs
rep.urls <- unique(cursci$rep.study.article.URL)
