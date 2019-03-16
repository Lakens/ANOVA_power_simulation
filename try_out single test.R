library(mvtnorm)
library(afex)
library(emmeans)
library(ggplot2)
library(gridExtra)
library(reshape2)
source("effect_size_d_independent_function.R")
source("effect_size_d_paired_function.R")
source("conf_limits_nct.R")

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

round_dig <- 4 #Set digits to which you want to round the output. 

alpha<-0.05

string <- design_result$string #String used to specify the design

factornames <- design_result$factornames #Get factor names

# Specify the parameters you expect in your data (sd, r for within measures)

#number of subjects you will collect (for each between factor) 
# For an all within design, this is total N
# For a 2b*2b design, this is the number of people in each between condition, so in each of 2*2 = 4 groups 

n<-design_result$n

# specify population means for each condition (so 2 values for 2b design, 6 for 2b*3w, etc) 
mu = design_result$mu # population means - should match up with the design

sd <- design_result$sd #population standard deviation (currently assumes equal variances)
r <- design_result$r # correlation between within factors (currently only 1 value can be entered)

#indicate which adjustment for multiple comparisons you want to use (e.g., "holm")
p_adjust <- design_result$p_adjust

###############
# 2. Create Dataframe based on Design ----
###############

#Count number of factors in design
factors <- design_result$factors

#Specify within/between factors in design: Factors that are within are 1, between 0
design <- design_result$design

sigmatrix <- design_result$sig

#Create the data frame. This will be re-used in the simulation (y variable is overwritten) but created only once to save time in the simulation
df <- design_result$df

###############
# 3. Specify factors for formula ----
###############

frml1 <- design_result$frml1 
frml2 <- design_result$frml2

aov_result<- suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                                       data=df,
                                       anova_table = list(es = "pes", p_adjust_method = p_adjust)) }) #This reports PES not GES

# pairwise comparisons
pc <- suppressMessages({pairs(emmeans(aov_result, frml2), adjust = p_adjust) })



#How many possible planned comparisons are there (to store p and es)
possible_pc <- (((prod(as.numeric(strsplit(string, "\\D+")[[1]])))^2)-prod(as.numeric(strsplit(string, "\\D+")[[1]])))/2

#create empty dataframe to store simulation results
#number of columns if for ANOVA results and planned comparisons, times 2 (p and es)
sim_data <- as.data.frame(matrix(ncol = 2*(2^factors-1)+2*possible_pc, nrow = 1))

#Dynamically create names for the data we will store
names(sim_data) = c(paste("anova_",
                          rownames(aov_result$anova_table), 
                          sep=""), 
                    paste("anova_es_", 
                          rownames(aov_result$anova_table), 
                          sep=""), 
                    paste("paired_comparison_", 
                          pc@grid[["contrast"]], 
                          sep=""), 
                    paste("d_", 
                          pc@grid[["contrast"]], 
                          sep=""))

#START CALCULATION 

#We simulate a new y variable, melt it in long format, and add it to the df (surpressing messages)
df$y<-suppressMessages({melt(as.data.frame(rmvnorm(n=n,
                                                   mean=mu,
                                                   sigma=as.matrix(sigmatrix))))$value
})
str(df)
df$subject <- as.factor(df$subject)
# We perform the ANOVA using AFEX
aov_result<-suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                                      data=df,
                                      anova_table = list(es = "pes", 
                                                         p_adjust_method = p_adjust,
                                                         correction = "none",
                                                         check_contrasts = TRUE))}) #This reports PES not GES
#look at aov result
aov_result
# pairwise comparisons
pc <- suppressMessages({pairs(emmeans(aov_result, frml2), adjust = p_adjust)})
#look at pc
pc


str(df)
x <- 
df2 <- subset(df, factor1 != "factor1_level1")
df2 <- droplevels(df2)
str(df2$factor1)
t.test(y ~factor1, data=df2, paired = TRUE)
t.test(y ~factor1, data=df2, var = TRUE)

emmeans(aov_result, pairwise~factor1)


# store p-values and effect sizes for calculations and plots.
sim_data[1,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                  aov_result$anova_table[[5]], #partial eta squared
                  as.data.frame(summary(pc))$p.value, #p-values for paired comparisons
                  ifelse(as.data.frame(summary(pc))$df == n-1, #if df = n-1 (means within factor)
                         as.data.frame(summary(pc))$t.ratio/sqrt(n)*(1-(3/(4*(n-1)-1))), #Cohen's dz for within # g correction *(1-(3/(4*(n-1)-1)))
                         (2 * as.data.frame(summary(pc))$t.ratio)/sqrt(2*n)*(1-(3/(4*(2*n-2)-1))))) #Cohen's d for between # g correction *(1-(3/(4*(2*n-2)-1)))


sim_data