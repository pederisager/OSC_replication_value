#### load packages 
library(tidyverse)
library(plotly)
library(gridExtra)
library(MASS)
library(viridis)



#### define function to calculate RV formula
repVal <- function(c, y, n) {
  ## c = current citation count
  ## y = years since publication
  ## n = sample size
  
  vz <- 1/(n-3) # Fisher's Z variance
  RV <- c/y*vz
  RV
}



#### Plot formula behavior for range of input values on heatmap ####

set.seed(1)  # set seed to ensure reproducibility of reported values and plots

c.heat <- seq(1, 200, by=2)  # generate citation count values
y.heat <- 10  # for simplicity, years since publication is held constant. This number could also be varied. For demonstrating formula behavior over a range of input, varying C, Y, or C/Y produce the same result.
n.heat <- seq(11, 110)  # generate sample size values that will be used to calculate Vz
rv.heat <- sapply(c.heat, function(x) {  # calculate formula replication value for all combinarions of input parameters
  repVal(c = x, y = y.heat, n = n.heat)
  })
df.heat <- expand.grid("C" = c.heat, "Y" = y.heat, "n" = n.heat)
df.heat$CbyY <- df.heat$C / df.heat$Y  # log average yearly citations in separate column for plotting purposes
df.heat$rv <- apply(df.heat, 1, function(x) {
  repVal(c = x["C"], y = x["Y"], n = x["n"])
})

 ggplot(df.heat, aes(x = n, y = CbyY)) +  # plot replication value on a heatmap as a function of sample size and average yearly citation rate
    geom_raster(aes(fill = rv), interpolate = FALSE) +
    scale_y_continuous(name = "Average yearly citation rate") +
    scale_x_continuous(name = "Sample size", limit = c(11, 60)) +
    scale_fill_viridis(name = "Replication \nValue", option =  "B")  +
    theme_bw(base_size = 16) +  ggtitle(paste("Distribution of replication value over input parameters")) +
    theme(  # eliminate background, gridlines, chart border, and readjust legend
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(), 
      legend.position = c(0.85, 0.3),
      legend.background = element_rect(fill="lightblue",
                                       size=0.5, linetype="solid", 
                                       colour ="purple"),
      plot.title = element_text(hjust = 0.5))

 

#### Simulate input values plot contribution of individual input parameters ####

set.seed(2)  # set seed to ensure reproducibility of reported values and plots

c.sim <- sample(x = 0:500, size = 10000, replace = TRUE)  # citation counts, drawn randomly from uniform distribution
y.sim <- sample(x = 1:70, size = 10000, replace = TRUE)  # years since publication, drawn randomly from uniform distribution
n.sim <- sample(x = 4:500, size = 10000, replace = TRUE)  # sample sizes, drawn randomly from uniform distribution
rv.sim <- repVal(c = c.sim, y = y.sim, n = n.sim)  # calculate replication value for all random df(c, y, n) trios. 
df.sim <- data.frame("C"= c.sim, "Y" = y.sim, "n" = n.sim, "rv" = rv.sim)  # combine variables in data frame

a <- ggplot(data = df.sim, aes(x = c.sim, y = log(rv.sim, 10))) +  # plot RV by citation count
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, size = 2) +
  theme_bw() + 
  labs(x = "Number of citations", y = expression(log[10]("replication value")), title = "A")
b <- ggplot(data = df.sim, aes(x = y.sim, y = log(rv.sim, 10))) +  # plot RV by years since publication
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, size = 2) +
  theme_bw() + 
  labs(x = "Years since publication", y = "", title = "B")
c <- ggplot(data = df.sim, aes(x = n.sim, y = log(rv.sim, 10))) +  # plot RV by sample size
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, size = 2) +
  theme_bw() + 
  labs(x = "Sample size", y = "", title = "C")
grid.arrange(a,b,c, nrow = 1)  # combine plots horizontally in one figure


########### OLD ############


#### Replication value: Simulations to study value behavior
library(ggplot2)
library(gridExtra)
library(plot3D)
source("RV_functions.R")

## Simulate studies

simulations <- 10000

sim <- simStudies(simulations)
a <- qplot(data = sim, x = citations, geom = "histogram")
b <- qplot(data = sim, x = replications, geom = "bar")
c <- qplot(data = sim, x = r, geom = "histogram")
d <- qplot(data = sim, x = n, geom = "histogram")
e <- qplot(data = sim, x = se, geom = "histogram")
f <- qplot(data = sim, x = errorrate, geom = "bar")
g <- qplot(data = sim, x = altmetric, geom = "histogram")
h <- qplot(data = sim, x = pub.year, geom = "histogram")

grid.arrange(a,b,c,d,e,f,g,h, nrow = 2)

#### Test Brandt RV ####

sim <- simStudies(simulations)

rv <- rvBrandt(r = sim$r, n = sim$n,citations = sim$citations, replications = sim$replications, spielraum = 1.92218)

qplot(sim$citations, rv$rv) + theme_bw()
qplot(rv$precision, rv$rv) + theme_bw()

# Varying citations
sim.a <- simStudies(simulations)
sim.a$citations <- rep(c(1, 10, 100, 300, 800), simulations/5)
rv.a <- rvBrandt(r = sim.a$r, n = sim.a$n, citations = sim.a$citations, replications = sim.a$replications, spielraum = 1.92218)
a <- qplot(sim.a$citations, rv.a$rv) + theme_bw() + geom_hline(yintercept = 1)

# Varying replications
sim.b <- simStudies(simulations)
sim.b$replications <- rep(c(0, 1, 2, 3, 4), simulations/5)
rv.b <- rvBrandt(r = sim.b$r, n = sim.b$n,citations = sim.b$citations, replications = sim.b$replications, spielraum = 1.92218)
b <- qplot(sim.b$citations, rv.b$rv, col=sim.b$replications) + theme_bw() + geom_vline(xintercept = c(100, 300, 800)) + geom_hline(yintercept = 1)

# Varying n
sim.c <- simStudies(simulations)
sim.c$n <- rep(c(20, 50, 100, 300, 800), simulations/5)
rv.c <- rvBrandt(r = sim.c$r, n = sim.c$n,citations = sim.c$citations, replications = sim.c$replications, spielraum = 1.92218)
sim.c$rv <- rv.c$rv
c <- ggplot(data=sim.c, aes(x = as.factor(n), y = rv)) + geom_boxplot() + geom_hline(yintercept = 1) + theme_bw()

# Varying r
sim.d <- simStudies(simulations)
sim.d$n <- rep(c(50), simulations)
sim.d$r <- rep(c(0, .1, .25, .5, .99), simulations/5)
rv.d <- rvBrandt(r = sim.d$r, n = sim.d$n,citations = sim.d$citations, replications = sim.d$replications, spielraum = 1.92218)
sim.d$rv <- rv.d$rv
d <- ggplot(data=sim.d, aes(x = as.factor(r), y = rv)) + geom_boxplot() + geom_hline(yintercept = 1) + theme_bw()


grid.arrange(a, b, c, d, nrow = 2)

#### Test rvCAN ####

weight <- c(1,1,1)

sim <- simStudies(simulations)

sim$rv <- rvCAN(citations = sim$citations, altmetric = sim$altmetric, n = sim$n, weight = weight)

a <- qplot(sim$citations+sim$altmetric, sim$rv) + theme_bw()
b <- qplot(sim$n, sim$rv, col = sim$citations+sim$altmetric) + theme_bw() + labs(x = "n", y = "RV", color = "Impact")

# Varying citations
sim.a <- simStudies(simulations)
sim.a$citations <- rep(c(1, 10, 100, 300, 800), simulations/5)
sim.a$rv <- rvCAN(citations = sim.a$citations, altmetric = sim.a$altmetric, n = sim.a$n, weight = weight)
c <- qplot(sim.a$citations, sim.a$rv, geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

# Varying altmetrics
sim.b<- simStudies(simulations)
sim.b$altmetric <- rep(c(1, 10, 100, 300, 800), simulations/5)
sim.b$rv <- rvCAN(citations = sim.b$citations, altmetric = sim.b$altmetric, n = sim.b$n, weight = weight)
d <- qplot(sim.b$altmetric, sim.b$rv, geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

# Varying n
sim.c <- simStudies(simulations)
sim.c$n <- rep(c(20, 50, 100, 300, 800), simulations/5)
sim.c$rv <- rvCAN(citations = sim.c$citations, altmetric = sim.c$altmetric, n = sim.c$n, weight = weight)
e <- qplot(sim.c$n, sim.b$rv, geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)


grid.arrange(a, b, c, d, e, nrow = 2)

scatter3D(sim$citations+sim$altmetric, sim$n, sim$rv)


#### Test rvCApyN ####

sim <- simStudies(simulations)

sim$rv <- rvCApyN(citations = sim$citations, altmetric = sim$altmetric, n = sim$n, pub.year = sim$pub.year, log = FALSE)

a <- qplot(sim$citations+sim$altmetric, sim$rv, col = sim$n) + theme_bw() + labs(x = "Impact", y = "RV", color = "n")
b <- qplot(sim$n, sim$rv, col = sim$citations+sim$altmetric) + theme_bw() + labs(x = "n", y = "RV", color = "Impact")

# Varying citations
sim.a <- simStudies(simulations)
sim.a$citations <- rep(c(1, 10, 100, 300, 800), simulations/5)
sim.a$rv <- rvCApyN(citations = sim.a$citations, altmetric = sim.a$altmetric, n = sim.a$n, pub.year = sim.a$pub.year, log = FALSE)
c <- qplot(as.factor(sim.a$citations), sim.a$rv, geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

# Varying altmetrics
sim.b<- simStudies(simulations)
sim.b$altmetric <- rep(c(1, 10, 100, 300, 800), simulations/5)
sim.b$rv <- rvCApyN(citations = sim.b$citations, altmetric = sim.b$altmetric, n = sim.b$n, pub.year = sim.b$pub.year, log = FALSE)
d <- qplot(as.factor(sim.b$altmetric), sim.b$rv, geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

# Varying n
sim.c <- simStudies(simulations)
sim.c$n <- rep(c(20, 50, 100, 300, 800), simulations/5)
sim.c$rv <- rvCApyN(citations = sim.c$citations, altmetric = sim.c$altmetric, n = sim.c$n, pub.year = sim.c$pub.year, log = FALSE)
e <- qplot(as.factor(sim.c$n), sim.c$rv, geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)


grid.arrange(a, b, c, d, e, nrow = 2)

scatter3D(((sim$citations+sim$altmetric) / sim$pub.year), -sim$n, sim$rv)


#### Test rvCAP ####

weight <- c(1,1,1)

sim <- simStudies(simulations)

sim$rv <- rvCAP(citations = sim$citations, altmetric = sim$altmetric, se = sim$se, weight = weight)

a <- qplot(sim$citations+sim$altmetric, sim$rv, col = sim$se) + theme_bw() + labs(x = "Impact", y = "RV", color = "se")
b <- qplot(sim$se, sim$rv, col = sim$citations+sim$altmetric) + theme_bw() + labs(x = "se", y = "RV", color = "Impact")

# Varying citations
sim.a <- simStudies(simulations)
sim.a$citations <- rep(c(1, 10, 100, 300, 800), simulations/5)
sim.a$rv <- rvCAN(citations = sim.a$citations, altmetric = sim.a$altmetric, n = sim.a$se, weight = weight)
c <- qplot(as.factor(sim.a$citations), log(sim.a$rv), geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

# Varying altmetrics
sim.b<- simStudies(simulations)
sim.b$altmetric <- rep(c(1, 10, 100, 300, 800), simulations/5)
sim.b$rv <- rvCAN(citations = sim.b$citations, altmetric = sim.b$altmetric, n = sim.b$se, weight = weight)
d <- qplot(as.factor(sim.b$altmetric), log(sim.b$rv), geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

# Varying se
sim.c <- simStudies(simulations)
sim.c$se <- rep(c(0.1, 0.25, 0.6, 0.9, 0.999), simulations/5)
sim.c$rv <- rvCAN(citations = sim.c$citations, altmetric = sim.c$altmetric, n = sim.c$se, weight = weight)
e <- qplot(sim.c$se, log(sim.b$rv), geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)


grid.arrange(a, b, c, d, e, nrow = 2)


#### Test rvCApyP ####


sim <- simStudies(simulations, uniform = TRUE)

sim$rv <- rvCApyP(citations = sim$citations, pub.year = sim$pub.year, se = sim$se)

a <- qplot(sim$citations, log(sim$rv), col = sim$se) + theme_bw() + labs(x = "Impact", y = "RV", color = "se")
b <- qplot(sim$se, log(sim$rv), col = sim$citations) + theme_bw() + labs(x = "se", y = "RV", color = "Impact")

# Varying citations
sim.a <- simStudies(simulations)
sim.a$citations <- rep(c(1, 10, 100, 300, 800), simulations/5)
sim.a$rv <- rvCApyP(citations = sim.a$citations, pub.year = sim.a$pub.year, se = sim.a$se)
c <- qplot(as.factor(sim.a$citations), log(sim.a$rv), geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

# Varying altmetrics
sim.b<- simStudies(simulations)
sim.b$altmetric <- rep(c(1, 10, 100, 300, 800), simulations/5)
sim.b$rv <- rvCApyP(citations = sim.b$citations, pub.year = sim.b$pub.year, se = sim.b$se)
d <- qplot(as.factor(sim.b$altmetric), log(sim.b$rv), geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

# Varying se
sim.c <- simStudies(simulations)
sim.c$se <- rep(c(0.001, 0.01, 0.1, 0.3, 0.8), simulations/5)
sim.c$rv <- rvCApyP(citations = sim.c$citations, pub.year = sim.c$pub.year, se = sim.c$se)
e <- qplot(as.factor(sim.c$se), log(sim.c$rv), geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

# Plotting years since publication on x axis
f <- qplot(sim$pub.year, log(sim$rv), geom = "jitter") + theme_bw() + geom_hline(yintercept = 1)

grid.arrange(a, b, c, d, e, f, nrow = 2)

# 3D plot

library(plotly)
cit <- seq(0, 20, length.out = 100)
n <- seq(20, 100, length.out = 100)
rv <- sapply(cit, function(x) x * (1/(n-3)))

plot_ly(x = cit, y = n, z = rv, type = 'mesh3d') %>% 
  add_surface() %>%
  layout(
    scene = list(
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    zaxis = list(title = "")
    ), 
    font  = list(
    size = 12
    ))


## Simulate correlation data from scratch to investigate the relationship between N and RCApyP

library("MASS")
samples <- 900
r <- 0.5
data <- mvrnorm(n = samples, mu = c(0,0), Sigma = matrix(c(1,r,r,1), nrow = 2), empirical = FALSE)

cor <- cor(data)
z <- 0.5 * log((1+r) / (1-r))

x <- sapply(seq(4, 10000, by = 1), function(x) {
  vz <- 1/(x-3)
  sez <- sqrt(vz)
  ser <- (exp(1)^(2*sez)-1) / (exp(1)^(2*sez)+1)
  return(vz)
})

plot(seq(4, 10000, by = 1), 124*x)



## Simulate meta-aggregated data from scratch to investigate the relationship between N and RCApyP

x <- sapply(seq(10, 2000, by = 10), function(x) {
d.rep <- sqrt(rnorm(10, mean = 2, sd = 0.5)^2)
n.rep <- sample(4:x, 10, replace = TRUE)

var.rep <- (n.rep+n.rep)/(n.rep*n.rep) + d.rep^2 / (2*(n.rep+n.rep))
rep.cor.factor <- 1 - 3/(4*(n.rep+n.rep-2)-1)
g.rep <- d.rep * rep.cor.factor
gvar.rep <- rep.cor.factor^2 * var.rep
w.rep <- 1/gvar.rep
g.meta <- sum(w.rep * g.rep) / sum(w.rep)
var.meta <- 1/sum(w.rep)
a = 4 # only valid when n1 = n2
rvar.meta <- a^2 * var.meta / (g.meta^2 + a)^3
zvar.meta <- 0.5 * log((1+rvar.meta) / (1-rvar.meta))
zse.meta <- sqrt(zvar.meta)
rse.meta <- (exp(1)^(2*zse.meta) - 1) / (exp(1)^(2*zse.meta) + 1)

rv.meta <- sum.citpy.rep * rse.meta
ret <- c(sum(n.rep), rv.meta)
return(ret)
})
x <- t(x) 







r <- 0.995
z <- 0.5 * log((1+r) / (1-r))
vz <- 1/(4-3)
sez <- sqrt(vz)
ser <- (exp(1)^(2*sez)-1) / (exp(1)^(2*sez)+1)

moe_zu <- z + sez * -qnorm(.025)
moe_zl <- z - sez * -qnorm(.025)
moe_ru <- (exp(1)^(2*moe_zu)-1) / (exp(1)^(2*moe_zu)+1)
moe_rl <- (exp(1)^(2*moe_zl)-1) / (exp(1)^(2*moe_zl)+1)


x <- MASS::mvrnorm(n = 100, mu = c(0,0.1), Sigma = matrix(c(1,0.9,0.9,1), nrow = 2), empirical = TRUE)


#### calculate the probability of drawing a finding with "true" RV in the 90th percentile based on formula RV procedure
inspect10 <- function(r, n, pick, percentile) {
  cormat <- matrix(c(1, r, r, 1), nrow = 2)
  data <- as.data.frame(mvrnorm(n = n, mu = c(0, 0), Sigma = cormat))
  names(data) <- c("form.RV", "true.RV")
  data <- data[order(data$form.RV, decreasing = TRUE),]
  picked <- data[1:pick,]
  perc <- quantile(data$true.RV, percentile)
  any(picked[,2]>perc)
}

sims <- replicate(1000, inspect10(r = 0, n = 1000, pick = 10, percentile = .9))
mean(sims)

par(mfrow=c(2,1))
probs <- sapply(c(0, 0.1, 0.3, 0.5), function(r) {
  sapply(c(50, 100, 200, 400), function(n) {
    sims <- replicate(1000, inspect10(r = r, n = n, pick = 10, percentile = .95))
    mean(sims)
  })
})

rownames(probs) <- c(0, 0.1, 0.3, 0.5)
colnames(probs) <- c(50, 100, 200, 400)
matplot(colnames(probs), probs, type = 'l')
legend('bottomright', inset=.05, legend=rownames(probs), 
       pch=1, horiz=TRUE, col=1:5)


probs <- sapply(c(0, 0.1, 0.3, 0.5), function(r) {
  sapply(c(3, 5, 10, 20), function(pick) {
    sims <- replicate(1000, inspect10(r = r, n = 200, pick = pick, percentile = .95))
    mean(sims)
  })
})

rownames(probs) <- c(0, 0.1, 0.3, 0.5)
colnames(probs) <- c(3, 5, 10, 20)
matplot(colnames(probs), probs, type = 'l')
legend('bottomright', inset=.05, legend=rownames(probs), 
       pch=1, horiz=TRUE, col=1:5)

#### Simulate and plot correlation between formula RV and "true" RV

cormat <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
data <- as.data.frame(mvrnorm(n = 10000, mu = c(0, 0), Sigma = cormat))
names(data) <- c("form.RV", "true.RV")
data <- data[order(data$form.RV, decreasing = TRUE),]

a <- ggplot() +
  geom_histogram(fill = "blue",  aes(x = data[,2])) +
  geom_histogram(aes(x = data[1:20000, 2]), fill = "red")

cormat <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)
data <- as.data.frame(mvrnorm(n = 10000, mu = c(0, 0), Sigma = cormat))
names(data) <- c("form.RV", "true.RV")
data <- data[order(data$form.RV, decreasing = TRUE),]

b <- ggplot() +
  geom_histogram(fill = "blue",  aes(x = data[,2])) +
  geom_histogram(aes(x = data[1:20000, 2]), fill = "red")

grid.arrange(a, b, nrow = 2)

par(mfrow=c(2,2))
plot(as.data.frame(mvrnorm(n = 10000, 
                           mu = c(0, 0), 
                           Sigma = matrix(c(1, 0, 0, 1), nrow = 2))), 
     xlab = "formula RV", 
     ylab = "true RV")

plot(as.data.frame(mvrnorm(n = 10000, 
                           mu = c(0, 0), 
                           Sigma = matrix(c(1, 0.3, 0.3, 1), nrow = 2))), 
     xlab = "formula RV", 
     ylab = "true RV")

plot(as.data.frame(mvrnorm(n = 10000, 
                           mu = c(0, 0), 
                           Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2))), 
     xlab = "formula RV", 
     ylab = "true RV")

plot(as.data.frame(mvrnorm(n = 10000, 
                           mu = c(0, 0), 
                           Sigma = matrix(c(1, 0.8, 0.8, 1), nrow = 2))), 
     xlab = "formula RV", 
     ylab = "true RV")





