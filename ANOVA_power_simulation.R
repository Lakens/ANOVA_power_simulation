options(scipen=999)

library(mvtnorm)
library(afex)
library(lsmeans)
library(ggplot2)
library(psych)
library(tidyr)
require(gridExtra)
require(reshape2)
require(sjstats)

#String used to specify the design
# e.g., "2" for 1 factor, "2*2*2" for three factors
string <- "3b*3w" #String used to specify the design
factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))

#Specify within/between factors in design: Factors that are within are 1, between 0
design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]]
design <- as.numeric(design == "w") #if within design, set value to 1, otherwise to 0


#indicate which adjustment for multiple comparisons you want to use (e.g., "holm")
p_adjust <- "none" 

# specify means. 
#for 1x2: c(1, 2) so 2 means
#for 2x2: c(1, 2, 3, 4) so 4 means
#for 2x2x2: c(1, 2, 3, 4, 5, 6, 7, 8) so 8 means

#2x2
#mu = c(1, 2, 2, 2) # population means - should match up with the design
#2x2x2
#mu = c(1, 2, 2, 2, 1, 2, 2, 1) # population means - should match up with the design
#3x3
mu = c(1, 2, 2, 2, 1, 2, 2, 1, 2) # population means - should match up with the design
#3x3x3
#mu = c(2, 1, 2, 2, 2, 1, 2, 2, 1, 2, 2, 3, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 2, 2, 2) # population means - should match up with the design


if(prod(as.numeric(strsplit(string, "\\D+")[[1]])) != length(mu)){stop("the length of the vector with means does not match the study design")}

sd=1 #population standard deviations
r=0.5 # correlation between repeated measures
n<-5 #number of subjects
nsims = 100 # how many simulation replicates?

# create temp matrix to create one dataset, to get design ################################
# (this section is lazy but efficient programming)
sigmatrix <- matrix(r, length(mu),length(mu)) #create a matrix filled with value of correlation, nrow and ncol set to length in mu
diag(sigmatrix) <- sd # replace the diagonal with the sd
df <- as.data.frame(rmvnorm(n=n,
                            mean=mu,
                            sigma=sigmatrix))
df$subject<-as.factor(c(1:n))
df <- melt(df, 
           id.vars = "subject", 
           variable.name = "cond",
           value.name = "y")
for(j in 1:factors){
  df <- cbind(df, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                     1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                     sep="")), 
                                       each = n*prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                       times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}
names(df)[4:(3+factors)] <- letters[1:factors]

############################################
#Specify factors for formula ###############

#one factor
if(factors == 1 & sum(design) == 1){frml1 <- as.formula("y ~ a + Error(subject/a)")}
if(factors == 1 & sum(design) == 0){frml1 <- as.formula("y ~ a")}

if(factors == 2){
  if(sum(design) == 2){frml1 <- as.formula("y ~ a*b + Error(subject/a*b)")}
  if(sum(design) == 0){frml1 <- as.formula("y ~ a*b")}
  if(all(design == c(1, 0)) == TRUE){frml1 <- as.formula("y ~ a*b + Error(subject/a)")}
  if(all(design == c(0, 1)) == TRUE){frml1 <- as.formula("y ~ a*b + Error(subject/b)")}
}

if(factors == 3){
  if(sum(design) == 3){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*b*c)")}
  if(sum(design) == 0){frml1 <- as.formula("y ~ a*b*c")}
  if(all(design == c(1, 0, 0)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/a)")}
  if(all(design == c(0, 1, 0)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/b)")}
  if(all(design == c(0, 0, 1)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/c)")}
  if(all(design == c(1, 1, 0)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*b)")}
  if(all(design == c(0, 1, 1)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/b*c)")}
  if(all(design == c(1, 0, 1)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*c)")}
}

#Specify second formula used for plotting
if(factors == 1){frml2 <- as.formula("~a")}
if(factors == 2){frml2 <- as.formula("~a+b")}
if(factors == 3){frml2 <- as.formula("~a+b+c")}

############################################
#Specify factors for formula ###############
design_list <- unique(apply((df)[4:(3+factors)], 1, paste, collapse=""))

############################################
#Create Real Covariance Matrix##############

#Create empty matrix
sigmatrix <- data.frame(matrix(ncol=length(mu), nrow = length(mu)))

# General approach: For each factor in the list of the design, save the first item (e.g., a1b1)
# Then for each factor in the design, if 1, set number to wildcard
for(i1 in 1:length(design_list)){
  current_factor <- design_list[i1]
  for(i2 in 1:length(design)){
    #We set each number that is within to a wildcard, so that all within subject factors are matched
    if(design[i2] == 1){substr(current_factor, i2*2,  i2*2) <- "*"} 
  }
  sigmatrix[i1,]<-as.numeric(grepl(current_factor, design_list)) # compare factors that match with current factor, given wildcard, save list to sigmatrix
}

frml1 <- as.formula("y ~ a*b + Error(subject/(a*b))")
# We perform the ANOVA using AFEX
within.aov<-aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                    data=df,
                    anova_table = list(es = "pes", p_adjust_method = p_adjust)) #This reports PES not GES


diag(sigmatrix) <- sd # replace the diagonal with the sd
sigmatrix <- as.matrix(sigmatrix)



########################################################
#Set up dataframe for simulation results ###############

#How many possible planned comparisons are there (to store p and es)
possible_pc <- (((prod(as.numeric(strsplit(string, "\\D+")[[1]])))^2)-prod(as.numeric(strsplit(string, "\\D+")[[1]])))/2
#create empty dataframe
#number of columns if for ANOVA results and planned comparisons, times 2 (p and es)
sim_data <- as.data.frame(matrix(ncol = 2*(2^factors-1)+2*possible_pc, nrow = nsims))

############################################
#Start Simulation            ###############

frml1 <- as.formula("y ~ a*b + Error(subject/b)")
pb <- winProgressBar(title = "progress bar", min = 0, max = nsims, width = 300)
i=1
for(i in 1:nsims){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste( round(i/nsims*100, 0),
                                        "% done"))
  df <- as.data.frame(rmvnorm(n=n,
                             mean=mu,
                             sigma=sigmatrix))
  df$subject<-as.factor(c(1:n))
  df <- melt(df, 
            id.vars = "subject", 
            variable.name = "cond",
            value.name = "y")
  
  for(j in 1:factors){
    # Let's break this down - it's a bit tricky. First, we want to create a list of a1 a2 b1 b2 that will indicate the factors. 
    # We are looping this over the number of factors.
    # This: as.numeric(strsplit(string, "\\D+")[[1]]) - takes the string used to specify the design and turn it in a list. 
    # we take the letters from the alfabet: paste(letters[[j]] and add numbers 1 to however many factors there as: 1:as.numeric(strsplit(string, "\\D+")[[1]])[j], sep="")
    # We this get e.g. ,a1 a2 - we repeat these each: n*(2^(factors-1)*2)/(2^j) and them times:  (2^j/2) to get a list for each factor
    # We then bind these together with the existing dataframe.
    df <- cbind(df, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                       1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                       sep="")), 
                                         each = n*prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                         times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
    ))))
  }
  names(df)[4:(3+factors)] <- letters[1:factors]
  # We perform the ANOVA using AFEX
  within.aov<-aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                      data=df,
                      anova_table = list(es = "pes", p_adjust_method = p_adjust)) #This reports PES not GES
  # pairwise comparisons
  pc <- pairs(emmeans(within.aov, frml2), adjust = p_adjust) #no adjustments
  # store p-values and effect sizes for calculations and plots.
  sim_data[i,] <- c(within.aov$anova_table[[6]], #p-value for ANOVA
                  within.aov$anova_table[[5]], #partial eta squared
                  as.data.frame(summary(pc))$p.value, #p-values for paired comparisons
                  as.data.frame(summary(pc))$t.ratio/sqrt(n)) #Cohen's dz
}

close(pb) #close the progress bar

############################################
#End Simulation              ###############


x<-within.aov[["lm"]][["terms"]][[2]]
pc@grid[["contrast"]]

#Dynamically create names for thedata we will store
names(sim_data) = c(paste("anova_p_",
                          within.aov$Anova$terms[-1], 
                          sep=""), 
                    paste("anova_es_", 
                          within.aov$Anova$terms[-1], 
                          sep=""), 
                    paste("paired_comparison_p_", 
                          pc@grid[["contrast"]], 
                          sep=""), 
                    paste("d_", 
                          pc@grid[["contrast"]], 
                          sep=""))

# melt the data into a long format for plots in ggplot2
require(reshape2)
power_anova = apply(as.matrix(sim_data[1:(2^factors-1)]), 2, 
              function(x) round(mean(ifelse(x < .05, 1, 0) * 100),2))
power_anova

plotData <- melt(sim_data[1:(2^factors-1)], value.name = 'p')

# plot each of the p-value distributions 
options(scipen = 999) # 'turn off' scientific notation
plt1 = ggplot(plotData, aes(x = p)) +
  scale_x_continuous(breaks=seq(0, 1, by = .1),
                     labels=seq(0, 1, by = .1)) +
  geom_histogram(colour = "darkgrey", fill = "white", breaks=seq(0, 1, by = .01)) +
  geom_vline(xintercept = 0.05, colour='red') +
  facet_grid(variable ~ .) +
  labs(x = expression(p)) +
  theme(axis.text.x = element_text(color='black', size=7))
plt1

#effect sizes for each test in the ANOVA (partial eta-squared)
colMeans(sim_data[(2^factors):(2*(2^factors-1))])
#estimated power for each test in the ANOVA
apply(as.matrix(sim_data[1:(2^factors-1)]), 2, 
      function(x) round(mean(ifelse(x < .05, 1, 0) * 100),2))







# melt the data into a ggplot friendly 'long' format
require(reshape2)
p <- sim_data[2*(2^factors-1)+1:2*(2^factors-1)+2*possible_pc]

power = apply(as.matrix(sim_data[(2*(2^factors-1)+1):(2*(2^factors-1)+2*possible_pc)]), 2, 
              function(x) round(mean(ifelse(x < .05, 1, 0) * 100),2))
power

sim_data[2*(2^factors-1)+1:2*(2^factors-1)+2*possible_pc]

plotData <- melt(p, value.name = 'p')

# plot each of the p-value distributions 
options(scipen = 999) # 'turn off' scientific notation
plt2 = ggplot(plotData, aes(x = p)) +
  scale_x_continuous(breaks=seq(0, 1, by = .1),
                     labels=seq(0, 1, by = .1)) +
  geom_histogram(colour = "darkblue", fill = "white", breaks=seq(0, 1, by = .01)) +
  geom_vline(xintercept = 0.05, colour='red') +
  facet_grid(variable ~ .) +
  labs(x = expression(p)) +
  theme(axis.text.x = element_text(color='black', size=7))
plt2


es <- sim_data[4:6]
describe(sim_data)

es <- sim_data[13:18]
describe(es)





########New Code generate DF for between data
#######BETWEEN
remove(df)
df <- as.data.frame(rmvnorm(n=n,
                            mean=mu,
                            sigma=as.matrix(sigmatrix)))
df$subject<-as.factor(c(1:n))
df <- melt(df, 
           id.vars = "subject", 
           variable.name = "cond",
           value.name = "y")
for(j in 1:factors){
  df <- cbind(df, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                     1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                     sep="")), 
                                       each = n*prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                       times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}
names(df)[4:(3+factors)] <- letters[1:factors]

#all between, we add the correct subject numbers
df$subject<-as.factor(c(1:(n*prod(as.numeric(strsplit(string, "\\D+")[[1]])))))
