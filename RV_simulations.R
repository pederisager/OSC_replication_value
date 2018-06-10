
#### Replication value: Simulations to study value behavior
library(ggplot2)
library(gridExtra)
source("RV_functions.R")

## Simulate studies

simulations <- 100000

sim <- simStudies(simulations)
a <- qplot(data = sim, x = citations, geom = "histogram")
b <- qplot(data = sim, x = replications, geom = "bar")
c <- qplot(data = sim, x = r, geom = "histogram")
d <- qplot(data = sim, x = n, geom = "histogram")
e <- qplot(data = sim, x = se, geom = "histogram")
f <- qplot(data = sim, x = errorrate, geom = "bar")
g <- qplot(data = sim, x = altmetric, geom = "histogram")

grid.arrange(a,b,c,d,e,f,g, nrow = 2)

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

