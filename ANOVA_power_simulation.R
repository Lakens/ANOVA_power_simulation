options(scipen=999)

library(mvtnorm)
library(afex)
library(lsmeans)
library(ggplot2)
library(psych)
library(tidyr)
require(gridExtra)
require(reshape2)

#String used to specify the design
# e.g., "2" for 1 factor, "2*2*2" for three factors
string <- "3" #String used to specify the design
factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))
if(factors == 1){frml1 <- as.formula("y ~ a + Error(subject/a)")}
if(factors == 1){frml2 <- as.formula("~a")}
if(factors == 2){frml1 <- as.formula("y ~ a*b + Error(subject/a*b)")}
if(factors == 2){frml2 <- as.formula("~a+b")}
if(factors == 3){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*b*c)")}
if(factors == 3){frml2 <- as.formula("~a+b+c")}

#indicate which adjustment for multiple comparisons you want to use (e.g., "holm")
p_adjust <- "none" 

# specify means. 
#for 1x2: c(1, 2) so 2 means
#for 2x2: c(1, 2, 3, 4) so 4 means
#for 2x2x2: c(1, 2, 3, 4, 5, 6, 7, 8) so 8 means
mu = c(6, 2, 6) # population means - should match up with the design
sd=1 #population standard deviations
r=0.5 # correlation between repeated measures
n<-20 #number of subjects
nsims = 100 # how many simulation replicates?

#create matrix
sigmatrix <- matrix(r, length(mu),length(mu)) #create a matrix filled with value of correlation, nrow and ncol set to length in mu
diag(sigmatrix) <- sd # replace the diagonal with the sd


# 2*(2^factors-1)+2^factors*(2^factors-1) #determines how much we will calculate - p and eta, for each main effect and interaction, and then add all simple effects
sim_data <- as.data.frame(matrix(ncol = 2*(2^factors-1)+2^factors*(2^factors-1), nrow = nsims))
names(sim_data) = c("anova_p1", "anova_p2", "anova_p3", "es_1", "es_2", "es_3", "pc_p1", "pc_p2", "pc_p3", "pc_p4", "pc_p5", "pc_p6", "pc_es1", "pc_es2", "pc_es3", "pc_es4", "pc_es5", "pc_es6")

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
    df <- cbind(df, as.factor(unlist(rep(as.list(paste(letters[[j]], 1:as.numeric(strsplit(string, "\\D+")[[1]])[j], sep="")), each = n*(2^(factors-1)*2)/(2^j), times = (2^j/2)))))
  }
  names(df)[4:(3+factors)] <- letters[1:factors]
  # #not used
  # mod <- aov(y ~ X1 * X2 + Error(subject / (X1*X2)), qr=FALSE, data = a) 
  # 
  # 
  # require(psych)
  # describe(a$y)
  # 
  # require(ggplot2)
  # plot_means<-ggplot(a, aes(X1, y, fill=X2)) +
  #   geom_bar(position=position_dodge(), 
  #            stat="summary", 
  #            fun.y="mean")
  # plot_means
  # 
  #Normal ANOVA using AFEX
  within.aov<-aov_car(frml1, 
                      data=df,
                      anova_table = list(es = "pes", p_adjust_method = p_adjust)) #This reports PES not GES
  # within.aov
  
  # #Automatically applies Holm-Bonferroni correction
  # within.aov<-aov_car(y ~ X1*X2 + Error(subject/X1*X2), 
  #                     data=a,
  #                     anova_table = list(es = "pes", p_adjust_method = "holm")) #This reports PES not GES
  # within.aov
  # 
  # 
  # within.aov<-aov_car(y ~ X1*X2 + Error(subject/X1*X2), 
  #                     data=a)
  # within.aov
  
  #Another way to plot the means
  # emmip(within.aov, X2~X1, CIs = TRUE)
  
  # 2. obtain reference grid object (default is uses univariate model):
  # r1 <- emmeans(within.aov, ~X1 + X2)
  # r1

    #pairwise comparisons
  pc <- pairs(emmeans(within.aov, frml2), adjust = p_adjust) #no adjustments
  #pc <- pairs(r1, adjust = "holm") #holm adjustments
  
  # #p-values for ANOVA table
  # within.aov$anova_table[[6]]
  # #partial eta squared
  # within.aov$anova_table[[5]]
  # #p-values for paired comparisons
  # as.data.frame(summary(pc))$p.value
  # #Cohen's dz
  # as.data.frame(summary(pc))$t.ratio/sqrt(n)
  
  #Combine all data to be stored in a single row.
  sim_data[i,] <- c(within.aov$anova_table[[6]], #p-value for ANOVA
                    within.aov$anova_table[[5]], #partial eta squared
                    as.data.frame(summary(pc))$p.value, #p-values for paired comparisons
                    as.data.frame(summary(pc))$t.ratio/sqrt(n) #Cohen's dz
  )
}

close(pb)

# melt the data into a ggplot friendly 'long' format
require(reshape2)
p <- sim_data[1:3]

power = apply(as.matrix(p), 2, 
              function(x) round(mean(ifelse(x < .05, 1, 0) * 100),0))
power

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



# melt the data into a ggplot friendly 'long' format
require(reshape2)
p <- sim_data[7:12]

power = apply(as.matrix(p), 2, 
              function(x) round(mean(ifelse(x < .05, 1, 0) * 100),0))
power

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
