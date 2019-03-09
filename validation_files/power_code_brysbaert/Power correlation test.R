# You can use power calculators for this correlation test 
# Still to set the scene, it is good to check the simulation approach with the expected outcome
# for r = .2, we expect that we need a group of 194 data pairs to reach 80% power
# we use the built-in Pearson correlation test


# give sample sizes
N = 194

# give effect size r
r = .2

# give number of simulations
nSim = 10000

# give alpha level (two-tailed)
alpha = .05

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vector for the p-values
p <-numeric(nSim)

# open library MASS
library('MASS')

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2))
  x = data[, 1]  # standard normal (mu=0, sd=1)
  y = data[, 2]  # standard normal (mu=0, sd=1)
  p[i] <- cor.test(x,y)$p.value
  }
close(pb)#close progress bar

#supportH0 <- sum(bf<(1/threshold))/nSim
supportH0 <- sum(p>=alpha)/nSim
#supportH1 <- sum(bf>threshold)/nSim
supportH1 <- sum(p<alpha)/nSim

cat("\n")
cat("Power of the test is ",supportH1)
cat("\n")


hist(p, breaks=20)

