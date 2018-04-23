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
string <- "2*2*2" #String used to specify the design
factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))
if(factors == 1){frml1 <- as.formula("y ~ a + Error(subject/a)")}
if(factors == 1){frml2 <- as.formula("~a")}
if(factors == 2){frml1 <- as.formula("y ~ a*b + Error(subject/a*b)")}
if(factors == 2){frml2 <- as.formula("~a+b")}
if(factors == 3){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*b*c)")}
if(factors == 3){frml2 <- as.formula("~a+b+c")}
if(factors == 4){frml1 <- as.formula("y ~ a*b*c*d + Error(subject/a*b*c*d)")}
if(factors == 4){frml2 <- as.formula("~a+b+c+d")}

#indicate which adjustment for multiple comparisons you want to use (e.g., "holm")
p_adjust <- "none" 

# specify means. 
#for 1x2: c(1, 2) so 2 means
#for 2x2: c(1, 2, 3, 4) so 4 means
#for 2x2x2: c(1, 2, 3, 4, 5, 6, 7, 8) so 8 means
mu = c(1, 2, 2, 2, 1, 2, 2, 1) # population means - should match up with the design
sd=1 #population standard deviations
r=0.5 # correlation between repeated measures
n<-50 #number of subjects
nsims = 100 # how many simulation replicates?

#create matrix
sigmatrix <- matrix(r, length(mu),length(mu)) #create a matrix filled with value of correlation, nrow and ncol set to length in mu
diag(sigmatrix) <- sd # replace the diagonal with the sd


# 2*(2^factors-1)+2^factors*(2^factors-1) #determines how much we will calculate - p and eta, for each main effect and interaction, and then add all simple pairwise comparisons
#planned contrasts possible: 

#pairs comparisons between groups
possible_pc <- (((prod(as.numeric(strsplit(string, "\\D+")[[1]])))^2)-prod(as.numeric(strsplit(string, "\\D+")[[1]])))/2

 
sim_data <- as.data.frame(matrix(ncol = 2*(2^factors-1)+2*possible_pc, nrow = nsims))

pb <- winProgressBar(title = "progress bar", min = 0, max = nsims, width = 300)

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


omega_sq(within.aov)



within.aov<-aov(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                data=df) #This reports PES not GES

# load sample data
data(efc)

# fit linear model
fit <- aov(
  c12hour ~ as.factor(e42dep) + as.factor(c172code) + c160age,
  data = efc
)

eta_sq(fit)
omega_sq(fit)
eta_sq(fit, partial = TRUE)