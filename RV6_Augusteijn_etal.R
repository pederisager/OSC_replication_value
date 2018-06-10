
# R Code for Precision

not.reg <- 1  # Preregistered or not.0 = Preregistered
error <- 0  # Contains serious reporting error or not. 0 = Does not contain. 
Q <- c(125,153,45,153,25,37,85,68,96,43,8)  # SAmple size
p <- c(1.69e-18,3.54e-06,0.04407098,0.001,0.03044959,0.049745991,0.003071238,0.001935206,0.117205428,0.033841139,0.023)  # p values
cit <- c(18,17,15,45,42,12,75,31,13,95,54)  # Citations
year <- 9  # Years since published

help <- cbind(40*p,1)
minimum <- apply(help,1,min)

ind.f <- ifelse(p > 0.05,0,1)

rv <- (not.reg + 3*error + 40*1/sqrt(Q) + 10*sqrt(minimum)*ind.f) * (1+log2(1+cit/year))
