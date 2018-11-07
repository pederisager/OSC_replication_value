
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

sim$pubs.p.year <- log(sim$citations / (as.integer(format(Sys.Date(), "%Y")) - sim$pub.year))
scatter3D(sim$pubs.p.year, sim$n, sim$rv)

library(plotly)
x <- sapply(seq(0, 1000, length.out = 100), function(x) x * seq(0, 1, length.out = 100))
p <- plot_ly(x = seq(0, 1000, length.out = 100), y = seq(0, 1, length.out = 100), z = x) %>% add_surface()


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
return(ser)
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
