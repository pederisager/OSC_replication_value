
## Replication value formula - functions

# RV from Brandt et al. ####

rvBrandt <- function(r, n, citations, replications, spielraum) {
  z <- r * log((1+r)/(1-r)) # Transform r to fisher z
  vz <- 1/(n-3) # Approximated variance of z
  # CI calculattion for z
  zci_upp <- z + 1.96 * sqrt(vz)
  zci_low <- z - 1.96 * sqrt(vz)
  # transform CIs back to r values
  rci_upp <- (exp(2 * zci_upp) - 1) / (exp(2 * zci_upp) + 1)
  rci_low <- (exp(2 * zci_low) - 1) / (exp(2 * zci_low) + 1)
  # Calculate precision as interval/spielraum ratio
  rci <-rci_upp - rci_low
  precision <- rci / spielraum
  # Calculate the RVfor the example case
  rv <- log(base = 3, x = citations) - 3.19 + precision - replications
  values <- data.frame(z, vz, zci_upp ,zci_low, rci_upp, rci_low, rci, precision, rv)
  return(values)
}


# Rv based on citations (+ altmetric scores) and N of study ####

rvCAN <- function(citations, altmetric, n, weight=c(1,1,1)) {
  metric <- data.frame(c=citations, a=altmetric, n=n)
  metric.w <- data.frame(t(t(metric) * weight))
  
  rv <- (metric.w$c + metric.w$a) / metric.w$n
  
  return(rv)
}


# Rv based on citations per year + altmetric score per year and N of study. Optional log of input ####

rvCApyN <- function(citations, altmetric, pub.year, n, log = FALSE) {
  
  y.since.pub <- as.integer(format(Sys.Date(), "%Y")) - pub.year 
  y.since.pub[y.since.pub == 0] = 1 # Avoid 0s breaking calculations

  if (log == TRUE) {
    rv <- log((citations + altmetric) / y.since.pub) / log(n) # if y.since.pub = 0, will divide by 1. Thus, papers from this year and last year will be weighted the same.
  }else{
    rv <- ((citations + altmetric) / y.since.pub) / n
  }
  return(rv)
}

# RV based on citations (+ altmetric scores) and standard error (precision) of effect ####

rvCAP <- function(citations, altmetric, precision, weight=c(1,1,1)) { 
  metric <- data.frame(c=citations, a=altmetric, p=precision)
  metric.w <- data.frame(t(t(metric) * weight))
  
  rv <- (metric.w$c + metric.w$a) * metric.w$p
  
  return(rv)
}

# RV based on citations (+ altmetric scores) and standard error (precision) of effect ####

rvCApyP <- function(citations, pub.year, se, altmetric = FALSE) {
  
  y.since.pub <- as.integer(format(Sys.Date(), "%Y")) - pub.year 
  y.since.pub[y.since.pub == 0] = 1 # Avoid 0s breaking calculations
  
  if (altmetric == FALSE) {
    
    rv <- ((citations) / y.since.pub) * se
    
    return(rv)
    
  } else {
    
    rv <- ((citations + altmetric) / y.since.pub) * se
    
    return(rv) 
  }
}


# RV based on citations (+ altmetric scores) and proportion of tests in paper with statcheck errors ####

rvCASt <- function(citations, altmetric, errorrate, weight=c(1,1,1)) {
  metric <- data.frame(c=citations, a=altmetric, e=errorrate)
  metric.w <- data.frame(t(t(metric) * weight))
  
  rv <- (metric.w$c + metric.w$a) * metric.w$e
  
  return(rv)
}

# Function to create simulated data for RV functions

simStudies <- function(nsims, uniform = TRUE) {
  
  if (uniform == TRUE) {
    
    citations <- sample(x = 1:1000, size = nsims, replace = TRUE)
    replications <- rbinom(n = nsims, size = 1, prob = 0.5)
    r <- runif(n = nsims, min = 0, max = 1)
    n <- sample(x = 4:1000, size = nsims, replace = TRUE)
    se <- runif(n = nsims, min = 0, max = 1)
    errorrate <- sample(x = 1:10, size = nsims, replace = TRUE)
    altmetric <- sample(x = 1:1000, size = nsims, replace = TRUE)
    pub.year <- sample(1919:2018, size = nsims, replace = TRUE)
    sim <- data.frame(citations, replications, r, n, se, errorrate, altmetric, pub.year) }
  else{
    citations <- sample(x = 1:1000, size = nsims, replace = TRUE, prob = (1000:1)^2)
    replications <- rbinom(n = nsims, size = 1, prob = 0.01)
    r <- runif(n = nsims, min = 0, max = 1)
    n <- sample(x = 10:1000, size = nsims, replace = TRUE, prob = (1000:10)^2)
    se <- runif(n = nsims, min = 0, max = 1)
    errorrate <- rbinom(n = nsims, size = 10, prob = 0.2)
    altmetric <- sample(x = 1:1000, size = nsims, replace = TRUE, prob = (1000:1)^2)
    pub.year <- sample(1919:2018, size = nsims, replace = TRUE, prob = (1:100)^2)
    sim <- data.frame(citations, replications, r, n, se, errorrate, altmetric, pub.year)
  }
  return(sim)
}