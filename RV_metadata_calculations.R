
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

master.df$rvCApyN <- rvCApyN(citations = master.df$x_crcited, altmetric = master.df$x_altscore, )

#### Stats and values bases on replication value calculations ####

## Get unique replication URLs
rep.urls <- unique(cursci$rep.study.article.URL)





#### Applying the RV formula to studies from the literature ####

## Case study - The Stroop effect 

# Stroop (1935), study 2

m.int <- 110.3
m.con <- 63.3
sd.int <- 18.8
sd.con <- 10.8
n <- 100

m.diff <- m.int - m.con
sd.pooled <- sqrt( ( (n - 1) * sd.int^2 + (n - 1) * sd.con^2) / (n+n-2) )

d.orig <- m.diff / sd.pooled

var.orig <- (n+n)/(n*n) + d.orig^2 / (2*(n+n)) # The variance is calculated without taking inter-trial correlation into account. However, using SDs and means yields only slightly different estimates, accoring to Dunlap et al. 
se.orig <- sqrt(var.orig)

# Converting from d to g
cor.factor <- 1 - 3/(4*(n+n-2)-1)
g.orig <- d.orig * cor.factor
gvar.orig <- cor.factor^2 * var.orig
gse.orig <- sqrt(gvar.orig)

# Converting from g to r
a = 4 # only valid when n1 = n2
rvar.orig <- a^2 * gvar.orig / (g.orig^2 + a)^3
zvar.orig <- 0.5 * log((1+rvar.orig) / (1-rvar.orig))
zse.orig <- sqrt(zvar.orig)
rse.orig <- (exp(1)^(2*zse.orig) - 1) / (exp(1)^(2*zse.orig) + 1)

# Calculating replication value
rv.orig <- rvCApyP(citations = 6874, pub.year = 1935, se = rse.orig)



