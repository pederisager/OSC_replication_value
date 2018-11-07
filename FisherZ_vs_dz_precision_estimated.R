
#------------------------------------------------------------------
# The following scripts calculates the standard error for repeated
# measures Hdges g, for different sample sized, inter-item 
# correlations, and effect sizes. It then compares these estimates
# to the standard error as estimated by Fisher Z. 
#
# The formulas used in this script are incorporated from:
# Borenstein, et al., (2009), Introduction to Meta-Analysis.
#------------------------------------------------------------------

# load packages
library(tidyverse)
library(gridExtra)

# Run calculations for 3 different mean differences.
for (difference in c(0.1, 1, 3)) {

# Define input parameters
n <- 10:300
diff <- difference
sd_diff <- 1
r <- seq(0.01, 0.99, length.out = 10)

# Compute standard error for Fisher's Z
var_z <- 1/(n-3)        # Borenstein 6.3 (assuming number of pairs as n!)
se_z <- sqrt(var_z)     # Borenstein 6.4

# Compute standard error for repeated measures d, for 
se_g <- sapply(r, function(r) {
  # SE in dz
  sd_within <- sd_diff/sqrt(2*(1-r))          # Borenstein 4.27
  d <- diff / sd_within                       # Borenstein 4.26
  var_d <- ( (1/n)+(d^2/(2*n)) * (2*(1-r)) )  # Borenstein 4.28
  # convert to g
  j = 1-3/(4*(n-1)-1)   # Borenstein 4.22
  var_g <- j^2 * var_d  # Borenstein 4.24
  se_g <- sqrt(var_g)   # Borenstein 4.25
})

# reshape the data to long format
se_g <- as.data.frame(se_g)
names(se_g) <- paste("r=", round(seq(0.01, 0.99, length.out = 10), 2), sep = "")
se_g$n <- n
se_g$z <- se_z
se_g <- gather(se_g, corr, se_g, 1:10)

# Plot standard error for different values of inter-trial correlation. 
assign(paste("d", diff, sep = ""), 
       ggplot(se_g, aes(n, se_g, col = corr)) + 
         geom_line() + 
         geom_line(aes(y = z), size = 1, col = "black", linetype = 2) +
         theme_bw() + 
         ggtitle(paste("mean diff =", diff))
)

}

# Plot standard error estimates for three different values of d.
# The stippled black line represents standard error of Fisher's Z. 
grid.arrange(d0.1, d1, d3)
