# You can use power calculators for this t test for unrelated samples
# Still to set the scene, it is good to check the simulation approach with the expected outcome
# for d = .4, we expect that we need two groups of 99 participants to reach 80% power
# we use the built-in t-test command

# give sample sizes
N1 = 99
N2 = N1

# give effect size d
d = .4

# give number of simulations
nSim = 10000

# give alpha level (two-tailed)
alpha = .05

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

p <-numeric(nSim)

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  x<-rnorm(n = N1, mean = 0, sd = 1)
  y<-rnorm(n = N2, mean = d, sd = 1) 
  p[i] <- t.test(x,y)$p.value
  }
close(pb)#close progress bar

#supportH0 <- sum(bf<(1/threshold))/nSim
supportH0 <- sum(p>=alpha)/nSim
#supportH1 <- sum(bf>threshold)/nSim
supportH1 <- sum(p<alpha)/nSim

cat("Power of the test is ",supportH1)

hist(p, breaks=20)

