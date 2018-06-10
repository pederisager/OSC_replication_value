
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



# Example calculation of RV from the formulas stated in the Google doc

r <- 0.2
n = 100
citations <- 100
replications <- 1  # Only direct replications of the effect counts
spielraum <- 1.92218  # (maximum CI that r can take, based on r = 0 and n = 4)

# Transform r to fisher z
z <- r * log((1+r)/(1-r))

# Approximated variance of z
vz <- 1/(n-3)

# CI calculattion for z
zci_upp <- z + 1.96 * sqrt(vz)
zci_low <- z - 1.96 * sqrt(vz)

# transform CIs back to r values
rci_upp <- (exp(2 * zci_upp) - 1) / (exp(2 * zci_upp) + 1)
rci_low <- (exp(2 * zci_low) - 1) / (exp(2 * zci_low) + 1)

# Calculate precision as interval/spielraum ratio
rci <-rci_upp - rci_low
rci
precision <- rci / spielraum
precision

# Calculate the RVfor the example case
RV <- log(base = 3, x = citations) - 3.19 + precision - replications
RV
