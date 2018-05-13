# Install the two functions from GitHub by running the code below:

source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_design.R")
source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_power.R")




repeat_sim <- function(x){ANOVA_power(ANOVA_design(string = "2b*2w",
                         n = x, 
                         mu = c(1.03, 1.21, 0.98, 1.01), 
                         sd = 1.03, 
                         r=0.87, 
                         p_adjust = "none"), 
            nsims = 100)
}

repeat_sim(x=20)