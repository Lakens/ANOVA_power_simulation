# Simulations to estimate the power of an repeated-measures ANOVA with three levels
# the effect between the two extreme groups is set to d = .4, the effect for the third group is d = .4 (see below for other situations)
# we use the built-in aov-test command

# give sample size
N = 75

# give effect size d
d1 = .4 #difference between the extremes
d2 = .4 #third condition goes with the highest extreme

# give the correlation between the conditions
r = .5

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA
alpha2 = .10/3 #alpha level for three post hoc one-tailed t-tests Bonferroni correction

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value omnibus ANOVA
p2 <-numeric(nSim) #p-value first post hoc test
p3 <-numeric(nSim) #p-value second post hoc test
p4 <-numeric(nSim) #p-value third post hoc test

# open library MASS
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r), c(r, 1, r), c(r, r, 1))

# define participant codes
part <- paste("part",seq(1:N))
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0), Sigma=rho)
  data[,2] = data[,2]+d1
  data[,3] = data[,3]+d2
  datalong = c(data[,1],data[,2],data[,3])
  conds= factor(rep(letters[24:26], each = N))
  partID = factor(rep(part, times = 3))
  output <-data.frame(partID,conds,datalong)
  test <- aov(datalong~conds + Error(partID/conds), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:conds'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- t.test(data[,1],data[,2], paired=TRUE)$p.value
  p3[i] <- t.test(data[,1],data[,3], paired=TRUE)$p.value
  p4[i] <- t.test(data[,2],data[,3], paired=TRUE)$p.value
  }
close(pb)#close progress bar

# results are as predicted when omnibus ANOVA is significant, t-tests are significant between x and y plus x and z; not significant between y and z
supportH1 <- sum((p1<alpha1) & (p2<alpha2) & (p3<alpha2) & (p4>=alpha2))/nSim

supportOMNI <- sum(p1<alpha)/nSim

cat("Power of the test is ",supportH1,"\n")
cat("Power of ANOVA is", supportOMNI,"\n")

hist(p1, breaks=20)



# Simulations to estimate the power of an repeated-measures ANOVA with three levels
# the effect between the two extreme groups is set to d = .4, the effect for the third group is d = .2 (see below for other situations)
# we use the built-in aov-test command

# give sample size
N = 290

# give effect size d
d1 = .4 #difference between the extremes
d2 = .2 #third condition in-between the two extremes

# give the correlation between the conditions
r = .5

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA
alpha2 = .10/3 #alpha level for three post hoc one-tailed t-tests Bonferroni correction

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value omnibus ANOVA
p2 <-numeric(nSim) #p-value first post hoc test
p3 <-numeric(nSim) #p-value second post hoc test
p4 <-numeric(nSim) #p-value third post hoc test

# open library MASS
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r), c(r, 1, r), c(r, r, 1))

# define participant codes
part <- paste("part",seq(1:N))
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0), Sigma=rho)
  data[,2] = data[,2]+d1
  data[,3] = data[,3]+d2
  datalong = c(data[,1],data[,2],data[,3])
  conds= factor(rep(letters[24:26], each = N))
  partID = factor(rep(part, times = 3))
  output <-data.frame(partID,conds,datalong)
  test <- aov(datalong~conds + Error(partID/conds), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:conds'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- t.test(data[,1],data[,2], paired=TRUE)$p.value
  p3[i] <- t.test(data[,1],data[,3], paired=TRUE)$p.value
  p4[i] <- t.test(data[,2],data[,3], paired=TRUE)$p.value
}
close(pb)#close progress bar

# results are as predicted when omnibus ANOVA is significant, t-tests are significant between x and y plus x and z; not significant between y and z
supportH1 <- sum((p1<alpha1) & (p2<alpha2) & (p3<alpha2) & (p4<alpha2))/nSim

supportOMNI <- sum(p1<alpha)/nSim

cat("Power of the test is ",supportH1,"\n")
cat("Power of ANOVA is", supportOMNI,"\n")

hist(p1, breaks=20)



# Simulations to estimate the power of an repeated-measures ANOVA with three levels
# the effect between the two extreme groups is set to dz = .4, the effect for the third group is dz = .4 (see below for other situations)
# dav is corrected for the correlation between the levels of the repeated measure
# we use the built-in aov-test command

# give sample size
N = 75

# give effect size d
dz1 = .4 #difference between the extremes
dz2 = .4 #third condition goes with the highest extreme

# give the correlation between the conditions
r = .9

# calculate dav
dav1 = dz1*sqrt(2*(1-r))
dav2 = dz2*sqrt(2*(1-r))

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA
alpha2 = .10/3 #alpha level for three post hoc one-tailed t-tests Bonferroni correction

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value omnibus ANOVA
p2 <-numeric(nSim) #p-value first post hoc test
p3 <-numeric(nSim) #p-value second post hoc test
p4 <-numeric(nSim) #p-value third post hoc test

# open library MASS
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r), c(r, 1, r), c(r, r, 1))

# define participant codes
part <- paste("part",seq(1:N))
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0), Sigma=rho)
  data[,2] = data[,2]+dav1
  data[,3] = data[,3]+dav2
  datalong = c(data[,1],data[,2],data[,3])
  conds= factor(rep(letters[24:26], each = N))
  partID = factor(rep(part, times = 3))
  output <-data.frame(partID,conds,datalong)
  test <- aov(datalong~conds + Error(partID/conds), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:conds'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- t.test(data[,1],data[,2], paired=TRUE)$p.value
  p3[i] <- t.test(data[,1],data[,3], paired=TRUE)$p.value
  p4[i] <- t.test(data[,2],data[,3], paired=TRUE)$p.value
}
close(pb)#close progress bar

# results are as predicted when omnibus ANOVA is significant, t-tests are significant between x and y plus x and z; not significant between y and z
supportH1 <- sum((p1<alpha1) & (p2<alpha2) & (p3<alpha2) & (p4>=alpha2))/nSim

supportOMNI <- sum(p1<alpha)/nSim

cat("Power of the test is ",supportH1,"\n")
cat("Power of ANOVA is", supportOMNI,"\n")

hist(p1, breaks=20)





# Simulations to estimate the power of an repeated-measures ANOVA with three levels
# the effect between the two extreme groups is set to dz = .4, the effect for the third group is dz = .4 (see below for other situations)
# dav is corrected for the correlation between the levels of the repeated measure
# we use the built-in aov-test command

# give sample size
N = 300

# give effect size d
dz1 = .4 #difference between the extremes
dz2 = .2 #third condition goes with the highest extreme

# give the correlation between the conditions
r = .9

# calculate dav
dav1 = dz1*sqrt(2*(1-r))
dav2 = dz2*sqrt(2*(1-r))

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA
alpha2 = .10/3 #alpha level for three post hoc one-tailed t-tests Bonferroni correction

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value omnibus ANOVA
p2 <-numeric(nSim) #p-value first post hoc test
p3 <-numeric(nSim) #p-value second post hoc test
p4 <-numeric(nSim) #p-value third post hoc test

# open library MASS
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r), c(r, 1, r), c(r, r, 1))

# define participant codes
part <- paste("part",seq(1:N))
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0), Sigma=rho)
  data[,2] = data[,2]+dav1
  data[,3] = data[,3]+dav2
  datalong = c(data[,1],data[,2],data[,3])
  conds= factor(rep(letters[24:26], each = N))
  partID = factor(rep(part, times = 3))
  output <-data.frame(partID,conds,datalong)
  test <- aov(datalong~conds + Error(partID/conds), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:conds'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- t.test(data[,1],data[,2], paired=TRUE)$p.value
  p3[i] <- t.test(data[,1],data[,3], paired=TRUE)$p.value
  p4[i] <- t.test(data[,2],data[,3], paired=TRUE)$p.value
}
close(pb)#close progress bar

# results are as predicted when omnibus ANOVA is significant, t-tests are significant between x and y plus x and z; not significant between y and z
supportH1 <- sum((p1<alpha1) & (p2<alpha2) & (p3<alpha2) & (p4<alpha2))/nSim

supportOMNI <- sum(p1<alpha)/nSim

cat("Power of the test is ",supportH1,"\n")
cat("Power of ANOVA is", supportOMNI,"\n")

hist(p1, breaks=20)


