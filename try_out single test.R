library(mvtnorm)
library(afex)
library(emmeans)
library(ggplot2)
library(gridExtra)
library(reshape2)

nsims = 100
K <- 3
mu <- c(0, 0.4, 0.4)
n <- 50
sd <- 1
r <- 0.5
string = paste(K,"w",sep="")
alpha_level <- 0.05
design_result <- ANOVA_design(string = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              p_adjust = "none",
                              labelnames = c("factor1", "level1", "level2", "level3"))


nsims = 100
mu <- c(0, 0.4, 0.4, 0)
n <- 50
sd <- 1
r <- 0.5
string = "2w*2b"
alpha_level <- 0.05
p_adjust = "none"
labelnames = c("age", "old", "young", "color", "blue", "red")
design_result <- ANOVA_design(string = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              p_adjust = p_adjust,
                              labelnames = labelnames)


simulation_result <- ANOVA_power(design_result, alpha = 0.05, nsims = 100)
