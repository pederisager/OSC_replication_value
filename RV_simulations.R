#-----------------------------------------------------------#
# The following script containts the code underlying the
# simulations reported in the Replication Value manuscript.
#
# The replication value formula used for this example:
# Yearly citation rate multiplied by variance of Fisher's Z.
#-----------------------------------------------------------#



#### load packages #### 
library(tidyverse)
library(plotly)
library(gridExtra)
library(MASS)
library(viridis)



#### define function to calculate RV formula #### 
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

c.heat <- seq(1, 60)  # generate citation count values
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
  geom_smooth(method = "lm", formula = y ~ log(x, 10), se = FALSE, size = 2) +
  theme_bw() + 
  labs(x = "Number of citations", y = expression(log[10]("replication value")), title = "A")
b <- ggplot(data = df.sim, aes(x = y.sim, y = log(rv.sim, 10))) +  # plot RV by years since publication
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", formula = y ~ log(x, 10), se = FALSE, size = 2) +
  theme_bw() + 
  labs(x = "Years since publication", y = expression(log[10]("replication value")), title = "B")
c <- ggplot(data = df.sim, aes(x = n.sim, y = log(rv.sim, 10))) +  # plot RV by sample size
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", formula = y ~ log(x, 10), se = FALSE, size = 2) +
  theme_bw() + 
  labs(x = "Sample size", y = expression(log[10]("replication value")), title = "C")
grid.arrange(a,b,c, nrow = 1)  # combine plots horizontally in one figure
