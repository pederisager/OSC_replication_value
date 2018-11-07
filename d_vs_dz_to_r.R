
set.seed(1)

## Simluate two random variables with an observed correlation and mean difference

x <- sapply(seq(0,1, length.out = 100), function(x) {
  
n <- 100
r = x
diff <- 1

x <- MASS::mvrnorm(n = n, 
                   mu = c(diff, 0),  # Because the sd of mvrnorm is 1, mean difference = Cohen's d
                   Sigma = matrix(c(1,r,r,1), nrow = 2), 
                   empirical = TRUE)

## Calculate d, SEd, dz, and SEdz for the data to show how they differ

m1 <- mean(x[,1])
m2 <- mean(x[,2])
sd1 <- sd(x[,1])
sd2 <- sd(x[,2])
diff <- mean(x[,1]) - mean(x[,2])

# Calculate d and SEd
sd_pooled <- sqrt( ( (n-1)*sd1^2 + (n-1)*sd2^2 ) / (n+n-2))
d <- diff / sd_pooled
var_d <- (n+n)/(n*n) + d^2/(2*(n+n))
se_d <- sqrt(var_d)

# Calculate dz and SEdz
sd_diff <- sd(x[,1]-x[,2])
r_prepost <- cor(x[,1],x[,2])
sd_within <- sd_diff / sqrt( 2 * (1-r_prepost) )
dz <- diff / sd_within
var_dz <- ( (1/n + d^2/(2*n)) * (2*(1-r_prepost)) )
se_dz <- sqrt(var_dz)

x <- c("d" = d, "se_d" = se_d, "dz" = dz, "se_dz" = se_dz, "r" = r)
return(x)
})

x <- t(x)

plot(x[,5], x[,4])

# From Lakens (2013): When r = 0.5 and the standard deviations in both groups of measurements
# are the same, Cohen's ds from a between-subjects design and Cohen's drm from a within-subjects
# design are identical.
d == dz 





