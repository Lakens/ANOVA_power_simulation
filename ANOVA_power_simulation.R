options(scipen=999)
rm(list=ls())  

###############
# 1. Specify Design and Simulation----
###############
# String used to specify the design
# Add numers for each factor with 2 levels, e.g., 2 for a factor with 2 levels
# Add a w after the number for within factors, and a b for between factors
# Seperate factors with a * (asteriks)
# Thus "2b*3w) is a design with 2 between levels, and 3 within levels

string <- "2w*2b" #String used to specify the design

# Specify the parameters you expect in your data (sd, r for within measures)

#number of subjects you will collect (for each between factor) 
# For an all within design, this is total N
# For a 2b*2b design, this is the number of people in each between condition, so in each of 2*2 = 4 groups 

n<-20

# specify population means for each condition (so 2 values for 2b design, 6 for 2b*3w, etc) 
mu = c(1.6, 1, 1, 1) # population means - should match up with the design

sd=1 #population standard deviation (currently assumes equal variances)
r=0.5 # correlation between within factors (currently only 1 value can be entered)

#indicate which adjustment for multiple comparisons you want to use (e.g., "holm")
p_adjust <- "none" 

# how many studies should be simulated? 100.000 is very accurate, 10.000 reasonable accurate, 10.000 somewhat accurate
nsims = 100

#Check if design an means match up - if not, throw an error and stop
if(prod(as.numeric(strsplit(string, "\\D+")[[1]])) != length(mu)){stop("the length of the vector with means does not match the study design")}

###############
# 2. Load libraries ----
###############

library(mvtnorm)
#Using developmental version of afex 
devtools::install_github("singmann/afex@master")
library(afex)
library(lsmeans)
library(ggplot2)
library(psych)
library(tidyr)
require(gridExtra)
require(reshape2)
require(sjstats)

###############
# 2. Create Dataframe based on Design ----
###############

#Count number of factors in design
factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))

#Specify within/between factors in design: Factors that are within are 1, between 0
design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]]
design <- as.numeric(design == "w") #if within design, set value to 1, otherwise to 0

sigmatrix <- matrix(r, length(mu),length(mu)) #create temp matrix filled with value of correlation, nrow and ncol set to length in mu
diag(sigmatrix) <- sd # replace the diagonal with the sd

#Create the data frame. This will be re-used in the simulation (y variable is overwritten) but created only once to save time in the simulation
df <- as.data.frame(rmvnorm(n=n,
                            mean=mu,
                            sigma=sigmatrix))

# colMeans(df)
# cor(df[1],df[2])
# sd(df[[1]])
# sd(df[[2]])

mean(df[[1]]-df[[2]])

df$subject<-as.factor(c(1:n)) #create temp subject variable just for merging
#Melt dataframe
df <- melt(df, 
           id.vars = "subject", 
           variable.name = "cond",
           value.name = "y")

# Let's break this down - it's a bit tricky. First, we want to create a list of a1 a2 b1 b2 that will indicate the factors. 
# We are looping this over the number of factors.
# This: as.numeric(strsplit(string, "\\D+")[[1]]) - takes the string used to specify the design and turn it in a list. 
# we take the letters from the alfabet: paste(letters[[j]] and add numbers 1 to however many factors there as: 1:as.numeric(strsplit(string, "\\D+")[[1]])[j], sep="")
# We this get e.g. ,a1 a2 - we repeat these each: n*(2^(factors-1)*2)/(2^j) and them times:  (2^j/2) to get a list for each factor
# We then bind these together with the existing dataframe.
for(j in 1:factors){
  df <- cbind(df, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                     1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                     sep="")), 
                                       each = n*prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                       times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}
#Rename the factor variables that were just created
names(df)[4:(3+factors)] <- letters[1:factors]

#Create subject colum (depends on design)
subject <- 1:n #Set subject to 1 to the number of subjects collected

for(j2 in length(design):1){ #for each factor in the design, from last to first
  #if w: repeat current string as often as the levels in the current factor (e.g., 3)
  #id b: repeat current string + max of current subject
  if(design[j2] == 1){subject <- rep(subject,as.numeric(strsplit(string, "\\D+")[[1]])[j2])}
  subject_length <- length(subject) #store current length - to append to string of this length below
  if(design[j2] == 0){
    for(j3 in 2:as.numeric(strsplit(string, "\\D+")[[1]])[j2]){
      subject <- append(subject,subject[1:subject_length]+max(subject))
    }
  }
}

#Overwrite subject columns in df
df$subject <- subject

mean(df[[1]]-df[[2]])


###############
# 3. Specify factors for formula ----
###############

#one factor
if(factors == 1 & sum(design) == 1){frml1 <- as.formula("y ~ a + Error(subject/a)")}
if(factors == 1 & sum(design) == 0){frml1 <- as.formula("y ~ a + Error(1 | subject)")}

if(factors == 2){
  if(sum(design) == 2){frml1 <- as.formula("y ~ a*b + Error(subject/a*b)")}
  if(sum(design) == 0){frml1 <- as.formula("y ~ a*b  + Error(1 | subject)")}
  if(all(design == c(1, 0)) == TRUE){frml1 <- as.formula("y ~ a*b + Error(subject/a)")}
  if(all(design == c(0, 1)) == TRUE){frml1 <- as.formula("y ~ a*b + Error(subject/b)")}
}

if(factors == 3){
  if(sum(design) == 3){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*b*c)")}
  if(sum(design) == 0){frml1 <- as.formula("y ~ a*b*c + Error(1 | subject)")}
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

###############
# 4. Create Covariance Matrix ----
###############

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

sigmatrix <- as.matrix(sigmatrix*r)
diag(sigmatrix) <- sd # replace the diagonal with the sd

#sigmatrix <- as.matrix(sigmatrix*r)

# We perform the ANOVA using AFEX
aov_result<-aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                    data=df,
                    anova_table = list(es = "pes", p_adjust_method = p_adjust)) #This reports PES not GES

# pairwise comparisons
pc <- pairs(emmeans(aov_result, frml2), adjust = p_adjust)

###############
# 5. Set up dataframe for simulation results
###############

#How many possible planned comparisons are there (to store p and es)
possible_pc <- (((prod(as.numeric(strsplit(string, "\\D+")[[1]])))^2)-prod(as.numeric(strsplit(string, "\\D+")[[1]])))/2

#create empty dataframe to store simulation results
#number of columns if for ANOVA results and planned comparisons, times 2 (p and es)
sim_data <- as.data.frame(matrix(ncol = 2*(2^factors-1)+2*possible_pc, nrow = nsims))

#Dynamically create names for the data we will store
names(sim_data) = c(paste("anova_p_",
                          rownames(aov_result$anova_table), 
                          sep=""), 
                    paste("anova_es_", 
                          rownames(aov_result$anova_table), 
                          sep=""), 
                    paste("paired_comparison_p_", 
                          pc@grid[["contrast"]], 
                          sep=""), 
                    paste("d_", 
                          pc@grid[["contrast"]], 
                          sep=""))

###############
# 6. Create plot of means to vizualize the design ----
###############

df_means <- data.frame(mu, SE = sd / sqrt(n))
for(j in 1:factors){
  df_means <- cbind(df_means, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                                 1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                                 sep="")), 
                                                   each = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                                   times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}

if(factors == 1){names(df_means)<-c("mu","SE","a")}
if(factors == 2){names(df_means)<-c("mu","SE","a","b")}
if(factors == 3){names(df_means)<-c("mu","SE","a","b","c")}

if(factors == 1){meansplot = ggplot(df_means, aes(y = mu, x = a))}
if(factors == 2){meansplot = ggplot(df_means, aes(y = mu, x = a, fill=b))}
if(factors == 3){meansplot = ggplot(df_means, aes(y = mu, x = a, fill=b)) + facet_wrap(  ~ c)}

meansplot = meansplot +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin = mu-SE, ymax = mu+SE), 
                position = position_dodge(width=0.9), size=.6, width=.3) +
  coord_cartesian(ylim=c((.7*min(mu)), 1.2*max(mu))) +
  theme_bw()
meansplot



###############
# 7. Start Simulation ----
###############

pb <- winProgressBar(title = "progress bar", min = 0, max = nsims, width = 300)
i=1
for(i in 1:nsims){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste( round(i/nsims*100, 0),
                                        "% done"))
  #We simulate a new y variable, melt it in long format, and add it to the df (surpressing messages)
  df$y<-suppressMessages({melt(as.data.frame(rmvnorm(n=n,
                             mean=mu,
                             sigma=sigmatrix)))$value
  })

  # We perform the ANOVA using AFEX
  aov_result<-suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                      data=df,
                      anova_table = list(es = "pes", p_adjust_method = p_adjust))}) #This reports PES not GES
  # pairwise comparisons
  pc <- pairs(emmeans(aov_result, frml2), adjust = p_adjust)
  # store p-values and effect sizes for calculations and plots.
  sim_data[i,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                  aov_result$anova_table[[5]], #partial eta squared
                  as.data.frame(summary(pc))$p.value, #p-values for paired comparisons
                  as.data.frame(summary(pc))$t.ratio/sqrt(n)) #Cohen's dz
}

close(pb) #close the progress bar

############################################
#End Simulation              ###############

t.test(df$y ~ df$a, paired = TRUE)

a1 <- df$y[1:n]
a2 <- df$y[(n+1):(2*n)]

a1-a2
mean(a1-a2)
cor(a1,a2)

###############
# 8. Plot Results ----
###############

# melt the data into a long format for plots in ggplot2

plotData <- melt(sim_data[1:(2^factors-1)], value.name = 'p')

SalientLineColor<-"#535353"
LineColor<-"#D0D0D0"
BackgroundColor<-"#F0F0F0"

# plot each of the p-value distributions 
options(scipen = 999) # 'turn off' scientific notation
plt1 = ggplot(plotData, aes(x = p)) +
  scale_x_continuous(breaks=seq(0, 1, by = .1),
                     labels=seq(0, 1, by = .1)) +
  geom_histogram(colour="#535353", fill="#84D5F0", breaks=seq(0, 1, by = .01)) +
  geom_vline(xintercept = 0.05, colour='red') +
  facet_grid(variable ~ .) +
  labs(x = expression(p)) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank()) + 
  theme(panel.background=element_rect(fill=BackgroundColor)) +
  theme(plot.background=element_rect(fill=BackgroundColor)) +
  theme(panel.border=element_rect(colour=BackgroundColor)) + 
  theme(panel.grid.major=element_line(colour=LineColor,size=.75)) + 
  theme(plot.title=element_text(face="bold",colour=SalientLineColor, vjust=2, size=20)) + 
  theme(axis.text.x=element_text(size=10,colour=SalientLineColor, face="bold")) +
  theme(axis.text.y=element_text(size=10,colour=SalientLineColor, face="bold")) +
  theme(axis.title.y=element_text(size=12,colour=SalientLineColor,face="bold", vjust=2)) +
  theme(axis.title.x=element_text(size=12,colour=SalientLineColor,face="bold", vjust=0)) + 
  theme(axis.ticks.x=element_line(colour=SalientLineColor, size=2)) +
  theme(axis.ticks.y=element_line(colour=BackgroundColor)) +
  theme(axis.line = element_line()) +
  theme(axis.line.x=element_line(size=1.2,colour=SalientLineColor)) +
  theme(axis.line.y=element_line(colour=BackgroundColor)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
plt1


# #Plot p-value distributions for simple comparisons
# # melt the data into a ggplot friendly 'long' format
# p_paired <- sim_data[(2*(2^factors-1)+1):(2*(2^factors-1)+possible_pc)]
# 
# plotData <- melt(p_paired, value.name = 'p')
# 
# # plot each of the p-value distributions 
# plt2 = ggplot(plotData, aes(x = p)) +
#   scale_x_continuous(breaks=seq(0, 1, by = .1),
#                      labels=seq(0, 1, by = .1)) +
#   geom_histogram(colour="#535353", fill="#84D5F0", breaks=seq(0, 1, by = .01)) +
#   geom_vline(xintercept = 0.05, colour='red') +
#   facet_grid(variable ~ .) +
#   labs(x = expression(p)) +
#   theme_bw() + 
#   theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank()) + 
#   theme(panel.background=element_rect(fill=BackgroundColor)) +
#   theme(plot.background=element_rect(fill=BackgroundColor)) +
#   theme(panel.border=element_rect(colour=BackgroundColor)) + 
#   theme(panel.grid.major=element_line(colour=LineColor,size=.75)) + 
#   theme(plot.title=element_text(face="bold",colour=SalientLineColor, vjust=2, size=20)) + 
#   theme(axis.text.x=element_text(size=10,colour=SalientLineColor, face="bold")) +
#   theme(axis.text.y=element_text(size=10,colour=SalientLineColor, face="bold")) +
#   theme(axis.title.y=element_text(size=12,colour=SalientLineColor,face="bold", vjust=2)) +
#   theme(axis.title.x=element_text(size=12,colour=SalientLineColor,face="bold", vjust=0)) + 
#   theme(axis.ticks.x=element_line(colour=SalientLineColor, size=2)) +
#   theme(axis.ticks.y=element_line(colour=BackgroundColor)) +
#   theme(axis.line = element_line()) +
#   theme(axis.line.x=element_line(size=1.2,colour=SalientLineColor)) +
#   theme(axis.line.y=element_line(colour=BackgroundColor)) + 
#   theme(plot.margin = unit(c(1,1,1,1), "cm"))
# plt2

###############
# 9. Sumary of power and effect sizes of main effects and contrasts ----
###############

#Main effects and interactions from the ANOVA
power = as.data.frame(apply(as.matrix(sim_data[(1:(2^factors-1))]), 2, 
                                   function(x) round(mean(ifelse(x < .05, 1, 0) * 100),3)))
es = as.data.frame(apply(as.matrix(sim_data[((2^factors):(2*(2^factors-1)))]), 2, 
                                function(x) round(mean(x),3)))

main_results <- data.frame(power,es)
names(main_results) = c("power","effect size")
main_results

#Data summary for contrasts
power_paired = as.data.frame(apply(as.matrix(sim_data[(2*(2^factors-1)+1):(2*(2^factors-1)+possible_pc)]), 2, 
              function(x) round(mean(ifelse(x < .05, 1, 0) * 100),2)))
es_paired = as.data.frame(apply(as.matrix(sim_data[(2*(2^factors-1)+possible_pc+1):(2*(2^factors-1)+2*possible_pc)]), 2, 
                                   function(x) round(median(x),2)))

pc_results <- data.frame(power_paired,es_paired)
names(pc_results) = c("power","effect size")
pc_results
