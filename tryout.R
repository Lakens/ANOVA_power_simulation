
options(scipen=999)

library(mvtnorm)
library(afex)
library(lsmeans)
library(ggplot2)
library(psych)
library(tidyr)
require(gridExtra)
require(reshape2)


mu = c(1, 0, 0, 0, 0, 0, 0, 0) # population means
sd=1 #population standard deviations
r=0.0 # correlation between repeated measures
n<-1 #number of subjects
p_adjust <- "none"

string <- "4*2"
factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))

#create matrix
sigmatrix <- matrix(r, length(mu),length(mu)) #create a matrix filled with value of correlation, nrow and ncol set to length in mu
diag(sigmatrix) <- sd # replace the diagonal with the sd

a <- as.data.frame(rmvnorm(n=n,
                           mean=mu,
                           sigma=sigmatrix))

a$subject<-as.factor(c(1:n))
a <- melt(a, 
          id.vars = "subject", 
          variable.name = "cond",
          value.name = "y")
# a$X1 <- as.factor(c(rep("A1", nrow(a)/2), 
#                     rep("A2", nrow(a)/2)))


df <- as.data.frame(rmvnorm(n=n,
                            mean=mu,
                            sigma=sigmatrix))
df$subject<-as.factor(c(1:n))
df <- melt(df, 
           id.vars = "subject", 
           variable.name = "cond",
           value.name = "y")


# prod(as.numeric(strsplit(string, "\\D+")[[1]]))/(as.numeric(strsplit(string, "\\D+")[[1]])[j])

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
                                       each = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                       times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}

prod(as.numeric(strsplit(string, "\\D+")[[1]][1:2]))
  
  
# First factor: each = prod*n/(factor level), times = 2^(j-1)
# SEcond factor: each = prod*n/(factor level), times = 2


names(df)[4:(3+factors)] <- letters[1:factors]
# We perform the ANOVA using AFEX
within.aov<-aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                    data=df,
                    anova_table = list(es = "pes", p_adjust_method = p_adjust)) #This reports PES not GES
# pairwise comparisons
pc <- pairs(emmeans(within.aov, frml2), adjust = p_adjust) #no adjustments
# store p-values and effect sizes for calculations and plots.





rep(as.list(paste(letters[[j]], 
                  1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                  sep="")), 
    each = n * (prod(as.numeric(strsplit(string, "\\D+")[[1]]))/(as.numeric(strsplit(string, "\\D+")[[1]])[1])^(j-1))/(as.numeric(strsplit(string, "\\D+")[[1]])[1]),
    times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:length(as.numeric(strsplit(string, "\\D+")[[1]]))]))


j=1
string <- "2*3"
rep(as.list(paste(letters[[j]], 
                  1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                  sep="")), 
    each = 3,
    times = 1)

j=1
string <- "4*2*3"
prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]) #each

prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:length(as.numeric(strsplit(string, "\\D+")[[1]]))]) #times


length(as.numeric(strsplit(string, "\\D+")[[1]]))