

#### Load data and set up data frames ####

# Load Pyschological bulletin data
pbul.df <- readRDS("examples/average_rv/psybull_meta_data.rds")

pbul.df$years_since_pub <- 2019 - pbul.df$x_pubyear  # Years since publication
pbul.df$cit_p_year <- pbul.df$x_crcited / pbul.df$years_since_pub  # average number of citations per year
pbul.df$RV <- pbul.df$cit_p_year * (1/(pbul.df$x_n - 3))  # RV per record
pbul.df <- pbul.df[!is.na(pbul.df$RV), ]  # Reduce data to those records for which RV can be calculated (i.e. has info on all input parameters and sample size >4)

# Aggregate citation data over duplicate article references
pbul.cit.df <- pbul.df[!duplicated(pbul.df$x_doi), c("x_doi", "x_crcited", "x_pubyear", "years_since_pub", "cit_p_year")]

wos.df <- read.csv("examples/average_rv/wos_data/psychology_alldata.csv")
wos.df <- wos.df[wos.df$PY>1993,]
wos.df <- wos.df[,colSums(is.na(wos.df))<nrow(wos.df)]
wos.df$years_since_pub <- 2019 - wos.df$PY
wos.df$cit_per_year <- wos.df$TC / wos.df$years_since_pub
wos.df$crossref_cit <- NA
# Extract crossref citations (this will run for hours on a normal computer/wifi connection)
library(rcrossref)
for (i in 1:length(wos.df$DI)) { 
  if (!wos.df$DI[i] == "") { 
    citations <- cr_citation_count(wos.df$DI[i]) 
    if (is.numeric(citations)) {
      wos.df$crossref_cit[i] <- citations
    }
  }
}
wos.df$cr_cit_per_year <-  wos.df$crossref_cit / wos.df$years_since_pub

# Save wos.df, now with crosref citations included
saveRDS(object = wos.df, file = "examples/average_rv/wos_data/wos_plus_crossref.rds")



#### Calculating psychology-average RV from Psychological Bulletin metadata ####

# Correlation between input parameters
cor.mat <- cor(pbul.df[, c("x_n", "x_crcited", "x_pubyear", "cit_p_year", "RV")], use = "complete.obs", method = "kendall")
library(corrplot)
corrplot(cor.mat, type = "upper", ax)

# Median RV from raw data
RV.iqr <- quantile(pbul.df$RV, probs = c(.25, .5,.75), na.rm = T)
hist(log(pbul.df$RV, 10))

# Median RV and interquartile range from median input parameters in pbul data
c.iqr <- quantile(publ.cit.df$x_crcited, probs = c(.25, .5,.75), na.rm = T)  # median citation count +/- IQR
y.iqr <- quantile(publ.cit.df$years_since_pub, probs = c(.25, .5,.75), na.rm = T)  # median years since publication +/- IQR
n.iqr <- quantile(pbul.df$x_n, probs = c(.25, .5,.75), na.rm = T)  # median sample size +/- IQR
cy.iqr <- quantile(pbul.df$cit_p_year, probs = c(.25, .5,.75), na.rm = T)

RV.m.iqr <- c.iqr/sort(y.iqr, decreasing = T) * (1/(sort(n.iqr, decreasing = T)-3))  # RV from medians

# Plot distributions
hist(log(publ.cit.df$x_crcited, 10))
hist(publ.cit.df$x_pubyear, breaks = 20)
hist(log(pbul.df$x_n, 10))
hist(log(pbul.df$cit_p_year, 10))
hist(log(pbul.df$RV, 10))


  
#### Calculating psychology-average RV from estimates of average sample size and yearly citation rate in WoS data ####

# Correlation between WoS and Crossref citations
wos.cr.c.corr.Kendall <- cor(wos.df$TC, wos.df$crossref_cit, use = "complete.obs", method = "kendall")
wos.cr.c.corr.Spearman <- cor(wos.df$TC, wos.df$crossref_cit, use = "complete.obs", method = "spearman")


# Median RV from median input parameters in Web of Science data

wos.c.iqr <- quantile(wos.df$TC, probs = c(.25, .5,.75), na.rm = T)  # median citation count +/- IQR (WoS)
cr.c.iqr <- quantile(wos.df$crossref_cit, probs = c(.25, .5,.75), na.rm = T)  # median citation count +/- IQR (WCrossref)
wos.y.iqr <- quantile(wos.df$years_since_pub, probs = c(.25, .5,.75), na.rm = T)  # median years since publication +/- IQR
wos.cy.iqr <- quantile(wos.df$cit_per_year, probs = c(.25, .5,.75), na.rm = T)  # median citations per year +/- IQR (WoS)
cr.cy.iqr <- quantile(wos.df$cr_cit_per_year, probs = c(.25, .5,.75), na.rm = T)  # median citations per year +/- IQR (Crossref)

wos.RV <- wos.cy.iqr * (1/(n.iqr-3))  # RV from medians of yearly citation rate and sample size +/- IQR (WoS citations)
cr.RV <- cr.cy.iqr * (1/(n.iqr-3))  # RV from medians of yearly citation rate and sample size +/- IQR (Crossref citations)

wos.RV.ms <- wos.c.iqr/wos.y.iqr * (1/(n.iqr-3))  # RV from medians of citation count, years since publication and sample size +/- IQR (WoS citations)
cr.RV.ms <- cr.c.iqr/wos.y.iqr * (1/(n.iqr-3))  # RV from medians of citation count, years since publication and sample size +/- IQR (WoS citations)

# Plot distributions
hist(wos.df$years_since_pub)
hist(log(wos.df$TC, 10))
hist(log(wos.df$crossref_cit, 10))
hist(log(wos.df$cit_per_year, 10))
hist(log(wos.df$cr_cit_per_year, 10))
