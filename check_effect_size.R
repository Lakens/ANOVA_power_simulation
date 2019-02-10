library(mvtnorm)
library(afex)
library(emmeans)
library(ggplot2)
library(gridExtra)
library(reshape2)

source("ANOVA_design.R")
source("ANOVA_power.R")


#simply repeated effect
design_result <- ANOVA_design(string = "2w",
                              n = 80, 
                              mu = c(1, 1.4), 
                              sd = 1, 
                              r=0.9, 
                              p_adjust = "none",
                              labelnames = c("age", "old", "young"))

simulation_result <- ANOVA_power(design_result, alpha = 0.05, nsims = 500)

#This should return a Cohen's dz of 0.9844.

#same design, now between effect
design_result <- ANOVA_design(string = "2b",
                              n = 80, 
                              mu = c(1, 1.4), 
                              sd = 1, 
                              r=0.9, 
                              p_adjust = "none",
                              labelnames = c("age", "old", "young"))

simulation_result <- ANOVA_power(design_result, alpha = 0.05, nsims = 500)

#This should return a Cohen's d of 0.4.

#now again within effect, but r = 0.5
design_result <- ANOVA_design(string = "2w",
                              n = 80, 
                              mu = c(1, 1.4), 
                              sd = 1, 
                              r=0.5, 
                              p_adjust = "none",
                              labelnames = c("age", "old", "young"))

simulation_result <- ANOVA_power(design_result, alpha = 0.05, nsims = 500)

#This should return a Cohen's dz of 0.4 (because the r = 0.5, same as between)