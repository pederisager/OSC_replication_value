
#----------------------------------------------------------#
# The following script containts the code underlying the
# Curate Science example case in the Replication Value 
# manuscript.
#
# The replication value used for this example:
# Yearly citation rate multiplied by variance of Fisher's Z.
#----------------------------------------------------------#


# Load dataset and relevant packages, and set working dir

setwd("C:/Users/20176239/Dropbox/jobb/PhD/Projects/2018_OSC_Replication_Value/examples/curate_science/")
library(rcrossref)
library(jsonlite)
library(tidyverse)
library(gridExtra)
library(stringr)
library(googlesheets)
library(canprot)
#source("../../../Meta-Data/analysis/MetaData_functions.R")
#source("../../../2018_OSC_Replication_Value/RV_functions.R")
curate_science <- read.csv("https://raw.githubusercontent.com/eplebel/science-commons/master/CS.rep.table.csv", na.strings = "", stringsAsFactors = FALSE)
cursci_orig_cr <- readRDS("curate_science_orig_study_crossref.Rds")



# Retrieve crossref metrics for original studies in the dataset (only needed once) ####

cursci_orig_cr <- cr_cn(dois = unique(curate_science$orig.study.article.DOI), "citeproc-json")  # Download DOI meta-data from crossref for each unique DOI
write_json(cursci_orig_cr, "curate_science_orig_study_crossref.json", pretty = TRUE)
saveRDS(cursci_orig_cr, "curate_science_orig_study_crossref.Rds")



#### Dataset set-up ####

# extract data from crossref data to be merged with curate_science
years <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`issued`$`date-parts`[1]), NA, x$`issued`$`date-parts`[1])))
rep.years <- as.numeric(str_extract(curate_science$rep.study.number, "[[:digit:]]+"))
citations <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`is-referenced-by-count`[1]), NA, x$`is-referenced-by-count`[1])))
doi <- unlist(lapply(cursci_orig_cr, function(x) ifelse(is.null(x$`DOI`[1]), NA, x$`DOI`[1]))) 
cr.df <- data.frame(orig.publ.year = years, orig.citations = citations, orig.study.article.DOI = doi)  
curate_science$orig.study.article.DOI <- tolower(curate_science$orig.study.article.DOI)  # set DOI characters in curate_science to lower case to match DOI formatting in cr.df
curate_science <- merge(curate_science, cr.df, by = "orig.study.article.DOI")  # Some DOIs do not match, leading to a smaller dataset.
curate_science$rep.publ.year <- rep.years

# Calculate replication value for original studies 

curate_science$orig.RV <- curate_science$orig.citations / ( (2019 - curate_science$orig.publ.year) * 1/(curate_science$orig.N-3) )

sum_n_orig <- aggregate(curate_science$orig.N, by=list(orig.study.number=curate_science$orig.study.number), FUN=mean)
sum_n_rep <- aggregate(curate_science$rep.N, by=list(orig.study.number=curate_science$orig.study.number), FUN=sum)
sum_n_rep$sum.N <- as.numeric(sum_n_rep$x + sum_n_orig$x)
sum_n_rep$x <- NULL

curate_science <- merge(x = curate_science, y = sum_n_rep, by = "orig.study.number", all = TRUE)



# Calculate replication value: average yearly citations * Fisher's Z variance

curate_science$orig.RV <- curate_science$orig.citations / (2019 - curate_science$orig.publ.year) * (1 / (curate_science$orig.N - 3))
curate_science$rep.RV <- curate_science$orig.citations / (2019 - curate_science$orig.publ.year) * (1 / (curate_science$orig.N + curate_science$rep.N - 3))  # Calculate RV of original plus replication for every replication
curate_science$sum.RV <- curate_science$orig.citations / (2019 - curate_science$orig.publ.year) * (1 / (curate_science$sum.N - 3))



# Aggregate results 

RVdata <- aggregate(curate_science$orig.N, by=list(orig.DOI=curate_science$orig.study.article.DOI,
                                                   orig.study.number=curate_science$orig.study.number,  
                                                   target.effect=curate_science$target.effect,
                                                   orig.RV=curate_science$orig.RV, 
                                                   sum.RV=curate_science$sum.RV, 
                                                   citations=curate_science$orig.citations,
                                                   y.cit=(curate_science$orig.citations/(2019-curate_science$orig.publ.year))
), FUN=mean)
names(RVdata)[length(names(RVdata))] <- "orig.N"
RVdata <- RVdata[order(-RVdata$orig.RV),]
RVdata$order <- 1:nrow(RVdata)



# Aggregate data further for plotting

names(RVdata)[4:5] <- c("original", "total")
RVdata <- gather(data = RVdata, key = "stage", value = "RV", original:total, factor_key = TRUE)
highest4 <- RVdata$orig.study.number[RVdata$RV %in% tail(sort(RVdata$RV[RVdata$stage == "original"]), 4)]  # Find 4 highest total replication values
RVdata$highRV <- ifelse((RVdata$orig.study.number %in% highest4 | RVdata$orig.study.number == "Stroop (1935) Study 2") & RVdata$stage == "original", RVdata$RV, NA)  # Mark variables to be labeled in the plot



# Derive relevant statistics and plots

## Load Pyschological bulletin data
pbul.df <- readRDS("examples/average_rv/psybull_meta_data.rds")
pbul.df$years_since_pub <- 2019 - pbul.df$x_pubyear  # Years since publication
pbul.df$cit_p_year <- pbul.df$x_crcited / pbul.df$years_since_pub  # average number of citations per year
pbul.df$RV <- pbul.df$cit_p_year * (1/(pbul.df$x_n - 3))  # RV per record
pbul.df <- pbul.df[!is.na(pbul.df$RV), ]  # Reduce data to those records for which RV can be calculated (i.e. has info on all input parameters and sample size >4)
## Aggregate citation data over duplicate article references
pbul.cit.df <- pbul.df[!duplicated(pbul.df$x_doi), c("x_doi", "x_crcited", "x_pubyear", "years_since_pub", "cit_p_year", "RV")]

## Median citation count of original findings
c.orig <- unique(RVdata[, c("orig.DOI", "citations")])[[2]]
c.orig.iqr <- quantile(x = c.orig, probs = c(.25, .5, .75))
c.orig.cles <- CLES(x = pbul.cit.df$x_crcited, y = c.orig)
### Plot
p.c <- ggplot(data = pbul.cit.df)  +
  geom_density(aes(x = log(x_crcited, 10)), fill = "red", alpha = 0.5, na.rm = T) + 
  geom_density(data = as.data.frame(c.orig), aes(x = log(c.orig, 10)), fill = "blue", alpha = 0.5, na.rm = T) +
  theme_classic(base_size = 16) + 
  labs(x = expression(log[10]("Citation count")), title = "A")

## Median average yearly citation count of original findings
cy.orig <- unique(RVdata[, c("orig.DOI", "y.cit")])[[2]]
cy.orig.iqr <- quantile(x = cy.orig, probs = c(.25, .5, .75))
cy.orig.cles <- CLES(x = pbul.cit.df$cit_p_year, y = cy.orig)
### Plot
p.cy <- ggplot(data = pbul.cit.df)  +
  geom_density(aes(x = log(cit_p_year, 10)), fill = "red", alpha = 0.5, na.rm = T) + 
  geom_density(data = as.data.frame(cy.orig), aes(x = log(cy.orig, 10)), fill = "blue", alpha = 0.5, na.rm = T) +
  theme_classic(base_size = 16) + 
  labs(x = expression(log[10]("Citations per year")), title = "B")

## Median sample size of original findings
n.orig <- RVdata$orig.N[RVdata$stage == "original"]
n.orig.iqr <- quantile(x = n.orig, probs = c(.25, .5, .75))
n.orig.cles <- CLES(x = pbul.df$x_n, y = n.orig)
### Plot 
p.n <- ggplot(data = pbul.df)  +
  geom_density(aes(x = log(x_n, 10)), fill = "red", alpha = 0.5, na.rm = T) + 
  geom_density(data = as.data.frame(n.orig), aes(x = log(n.orig, 10)), fill = "blue", alpha = 0.5, na.rm = T) +
  theme_classic(base_size = 16) + 
  labs(x = expression(log[10]("Sample size")), title = "C")

## Median replication value of original findings
RV.orig <- RVdata$RV[RVdata$stage == "original"]
RV.orig.iqr <- quantile(x = RV.orig, probs = c(.25, .5, .75))
RV.orig.cles <- CLES(x = pbul.df$RV, y = RV.orig)
## Median replication value, replications included
RV.tot <- RVdata$RV[RVdata$stage == "total"]
RV.tot.iqr <- quantile(x = RV.tot, probs = c(.25, .5, .75))
RV.tot.cles <- CLES(x = pbul.df$RV, y = RV.tot)
### Plot
p.RV <- ggplot(data = pbul.df)  +
  geom_density(aes(x = log(RV, 10)), fill = "red", alpha = 0.5, na.rm = T) + 
  geom_density(data = as.data.frame(RV.orig), aes(x = log(RV.orig, 10)), fill = "blue", alpha = 0.5, na.rm = T) +
  geom_density(data = as.data.frame(RV.tot), aes(x = log(RV.tot, 10)), fill = "green", alpha = 0.5, na.rm = T) +
  theme_classic(base_size = 16) + 
  labs(x = expression(log[10]("Replication value")), title = "D")
  

### Scatterplot of original and summary replication values, sorted by size
p.RV.sorted <- ggplot(data = RVdata, aes(x = order, y = RV)) + 
  geom_bar(aes(fill = stage), stat = "identity", width = 1) + 
  theme_classic() +
  theme(text = element_text(size=16), legend.position="none") +
  scale_fill_manual(values = c("blue", "green")) +
  labs(x = "Original study, sorted by original replication value", y = "Replication value", title = "E")


### Combine plots into grid for manuscript
grid.arrange(grid.arrange(p.c, p.cy, p.n, p.RV), p.RV.sorted, heights = c(2, 1))



