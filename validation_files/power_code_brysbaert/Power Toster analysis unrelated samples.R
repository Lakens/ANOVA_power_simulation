#TOSTER Power Analysis unrelated samples

library(TOSTER) 

D<-0.0 #Set the true effect size
n<-430 #Set sample size of your study (number in each group)
nSim<-5000 #Set number of simulations (it takes a while, be patient)
alpha<-.05 #Set the alpha-level 

# make vectors to save the two p-values
p <-numeric(nSim)

# create progress bar because it might take a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  x<-rnorm(n = n, mean = 0, sd = 1)
  y<-rnorm(n = n, mean = D, sd = 1) 
  tosttest  <- TOSTtwo(m1=mean(x),m2=mean(y),sd1=sd(x),sd2=sd(y),n1=n,n2=n,low_eqbound_d=-0.2, high_eqbound_d=0.2, 
                       alpha = 0.05, var.equal=TRUE, verbose=FALSE)
  p[i] <-max(tosttest$TOST_p1, tosttest$TOST_p2)
}
close(pb)#close progress bar

supportH0 <- sum(p<alpha)/nSim

cat("The probability of observing support for the null hypothesis is ",supportH0,"\n")

hist(p, breaks=20)


# The Toster package also allows you to do the power estimate mathematically
powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.2, high_eqbound_d=0.2)


