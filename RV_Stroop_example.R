

#--------------------------------------------------------#
# The following script containts the code underlying the
# Stroop example case in the Replication Value manuscript.
#
# The replication value currently used is:
# Yearly citation rate divided by adjusted sample size
#--------------------------------------------------------#

library(tidyverse)
source("RV_functions.R")

#  Replication value for study 2 in Stroop (1935)

citations <- 7230  # Based on crossref citation entry for DOI (10.1037/h0054651). Timestamp: 2018-11-07 11:53:40 CET
years_since_pub <- 2018 - 1935
n_within <- 100

a <- 2 # Each subject contributes data to two conditions
ml3_stroop <- read.table(file = "ML3StroopData.csv", header = TRUE, sep = ",")
r <- cor(ml3_stroop$MC, ml3_stroop$MI) # Within-subject correlation for stroop effect estimated from Many Labs 3 data.
n_adjusted <- (n_within*a)/(1-r)

RV_stroop_orig <- (citations / years_since_pub) / n_adjusted

#  Stroop (1935) replications reported in Verhaegen (1998), excluding non-close replications.

names.rep <- c("Cohn", "Hartman", "Houx", "Houx", "Kieley", "Kieley", "Kwong", "Li", "Panek", "Panek", "Salthouse", "Salthouse_Meinz", "Spieler", "Weir", "Weir", "Stroop")
pub.year <- c(1984, 1991, 1993, 1993, 1997, 1997, 1995, 1996, 1984, 1984, 1996, 1995, 1996, 1997, 1997, 1935)
n.rep <- c(20, 44, 42, 18, 16, 45, 82, 35, 19, 31, 40, 49, 27, 17, 24, 100)

stroop_reps <- data.frame("study" = names.rep, "pub_year" = pub.year, "n" = n.rep)
stroop_reps <- arrange(stroop_reps, pub.year)

plot((citations / years_since_pub) / ((sapply(1:length(stroop_reps$n), function(x) sum(stroop_reps$n[1:x]))*2)/(1-r)))











## OLD

source("RV_functions.R")

names.rep <- c("Cohn", "Hartman", "Houx", "Kieley", "Kwong", "Li", "Panek", "Salthouse", "Salthouse_Meinz", "Spieler", "Weir", "Stroop")
cit.rep <- c(79, 124, 178, 46, 58, 19, 25, 83, 147, 235, 6, 6874)
year.rep <- c(1984, 1991, 1993, 1997, 1995, 1996, 1984, 1996, 1995, 1996, 1997, 1935)
sum.citpy.rep <- sum(cit.rep / (2018 - year.rep))
d.rep <- c(2.54, 2.63, 2.37, 1.96, 2.25, 1.05, 2.01, 2.10, 2.43, 2.45, 2.07, 1.90, 1.14, 2.50, 1.66, 3.065681)
n.rep <- c(20, 44, 42, 18, 16, 45, 82, 35, 19, 31, 40, 49, 27, 17, 24, 100)


var.rep <- (n.rep+n.rep)/(n.rep*n.rep) + d.rep^2 / (2*(n.rep+n.rep))  # Borenstein eq: 4.20
rep.cor.factor <- 1 - 3/(4*(n.rep+n.rep-2)-1)
g.rep <- d.rep * rep.cor.factor
gvar.rep <- rep.cor.factor^2 * var.rep
w.rep <- 1/gvar.rep
g.meta <- sum(w.rep * g.rep) / sum(w.rep)
var.meta <- 1/sum(w.rep)
a = 4  # only valid when n1 = n2
rvar.meta <- a^2 * var.meta / (g.meta^2 + a)^3
zvar.meta <- 0.5 * log((1+rvar.meta) / (1-rvar.meta))
zse.meta <- sqrt(zvar.meta)
rse.meta <- (exp(1)^(2*zse.meta) - 1) / (exp(1)^(2*zse.meta) + 1)

rv.meta <- sum.citpy.rep * rse.meta