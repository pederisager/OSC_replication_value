

#--------------------------------------------------------#
# The following script containts the code underlying the
# Stroop example case in the Replication Value manuscript.
#
# The replication value used for this example:
# Yearly citation rate divided by adjusted sample size
#--------------------------------------------------------#

library(tidyverse)
library(ggplot2)

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

names.rep <- c("Cohn", "Hartman", "Houx 1", "Houx 2", "Kieley 1", "Kieley 2", "Kwong", "Li", "Panek 1", "Panek 2", "Salthouse", "Salthouse_Meinz", "Spieler", "Weir 1", "Weir 2", "Stroop")
pub.year <- c(1984, 1991, 1993, 1993, 1997, 1997, 1995, 1996, 1984, 1984, 1996, 1995, 1996, 1997, 1997, 1935)
n.rep <- c(20, 44, 42, 18, 16, 45, 82, 35, 19, 31, 40, 49, 27, 17, 24, 100)

stroop_reps <- data.frame("study" = names.rep, "pub_year" = pub.year, "n" = n.rep)
stroop_reps <- arrange(stroop_reps, pub.year)
stroop_reps$n_adj <- (stroop_reps$n*a)/(1-r)
stroop_reps$rv_seq <- citations / years_since_pub / (sapply(1:length(stroop_reps$n_adj), function(x) sum(stroop_reps$n_adj[1:x])))

plot((citations / years_since_pub) / (sapply(1:length(stroop_reps$n_adj), function(x) sum(stroop_reps$n_adj[1:x]))))

ggplot(data = stroop_reps, aes(x = 1:nrow(stroop_reps), y = rv_seq, label = study)) + 
  geom_line() +
  geom_text(angle = 30, hjust = "left") + 
  theme_classic() + 
  labs(x = "Replication order, sorted by publication year", y = "replication value")

