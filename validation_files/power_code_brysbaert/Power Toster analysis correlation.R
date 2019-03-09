#TOSTER Power Analysis correlations

library(TOSTER) 
library(MASS)

r<-0.0 #Set the true effect size of the correlation
N<-860 #Set sample size of your study (number in each group)
nSim<-5000 #Set number of simulations (it takes a while, be patient)
alpha<-.05 #Set the alpha-level 

# make vectors to save the two p-values
p <-numeric(nSim)

# create progress bar because it might take a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2))
  x = data[, 1]  # standard normal (mu=0, sd=1)
  y = data[, 2]  # standard normal (mu=0, sd=1)
  corr <- cor(x,y)
  tosttest  <- TOSTr(n=N,r=corr,low_eqbound_r=-0.1, high_eqbound_r=0.1, 
                       alpha = 0.05, verbose=FALSE)
  #print(tosttest)
  p[i] <-max(tosttest$TOST_p1, tosttest$TOST_p2)
}
close(pb)#close progress bar

supportH0 <- sum(p<alpha)/nSim

cat("The probability of observing support for the null hypothesis is ",supportH0,"\n")

hist(p, breaks=20)


# The Toster package also allows you to do the power estimate mathematically
powerTOSTr(alpha=0.05, statistical_power=0.8, low_eqbound_r=-0.1, high_eqbound_r=0.1)


