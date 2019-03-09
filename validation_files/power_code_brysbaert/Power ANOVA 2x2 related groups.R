# 1.
# Simulations to estimate the power of a 2x2 repeated-measures ANOVA 
# the effect of one variable is set to dz = .4; the other variable is a control variable that is not assumed to have an effect
# correlation = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command

# give sample size
N = 27

# give effect size d
d1 = .4 #difference of the first variable 
d2 = .0 #difference of the second variable

# give the correlation between the conditions
r = .5

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value factor 1
p2 <-numeric(nSim) #p-value factor 2
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r, r), c(r, 1, r, r), c(r, r, 1, r), c(r, r, r, 1))

# define participant codes
part <- paste("part",seq(1:N),sep="_")

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0, 0), Sigma=rho)
  data[,2] = data[,2]+d1
  data[,3] = data[,3]+d2
  data[,4] = data[,4]+d1+d2
  datalong = c(data[,1],data[,2],data[,3],data[,4])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 4))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/(fac1*fac2)), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:fac1'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[1]]
  p3[i] <- tests$'Error: partID:fac1:fac2'[[1]]$'Pr(>F)'[[1]]
  }
close(pb)#close progress bar

# results are as predicted when main effect of first repeated measures variable is significant
supportH1 <- sum(p1<alpha1)/nSim
cat("Power of the test is ",supportH1,"\n")

signfac2 <- sum(p2<alpha1)/nSim
cat("Significance factor 2 ",signfac2,"\n")

signint <- sum(p3<alpha1)/nSim
cat("Significance interaction ",signint,"\n")


# 2.
# Simulations to estimate the power of a 2x2 repeated-measures ANOVA 
# the effect of one variable is set to dz = .4; the other variable is a control variable that is not assumed to have an effect
# correlation = .9 and dav is decreased so that dz = .4 (see below for other situations)
# we use the built-in aov-test command

# give sample size
N = 27

# give effect size d
dz1 = .4 #difference of the first variable 
dz2 = .0 #difference of the second variable

# give the correlation between the conditions
r = .9

# calculate dav
dav1 = dz1*sqrt(2*(1-r))
dav2 = dz2*sqrt(2*(1-r))

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value factor 1
p2 <-numeric(nSim) #p-value factor 2
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r, r), c(r, 1, r, r), c(r, r, 1, r), c(r, r, r, 1))

# define participant codes
part <- paste("part",seq(1:N),sep="_")

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0, 0), Sigma=rho)
  data[,2] = data[,2]+dav1
  data[,3] = data[,3]+dav2
  data[,4] = data[,4]+dav1+dav2
  datalong = c(data[,1],data[,2],data[,3],data[,4])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 4))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/(fac1*fac2)), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:fac1'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[1]]
  p3[i] <- tests$'Error: partID:fac1:fac2'[[1]]$'Pr(>F)'[[1]]
}
close(pb)#close progress bar

# results are as predicted when main effect of first repeated measures variable is significant
supportH1 <- sum(p1<alpha1)/nSim
cat("Power of the test is ",supportH1,"\n")

signfac2 <- sum(p2<alpha1)/nSim
cat("Significance factor 2 ",signfac2,"\n")

signint <- sum(p3<alpha1)/nSim
cat("Significance interaction ",signint,"\n")





# 3.
# Simulations to estimate the power of a 2x2 repeated-measures ANOVA 
# Interaction: factor 1 has effect size of d = .4 at level 1 of factor 2 and d = .0 at the second level
# correlation = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command

# give sample size
N = 110

# give effect size d
d1 = .4 #half of the effect of the first variable 
d2 = .0 #half of the effect of the second variable

# give the correlation between the conditions
r = .5

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value factor 1
p2 <-numeric(nSim) #p-value factor 2
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r, r), c(r, 1, r, r), c(r, r, 1, r), c(r, r, r, 1))

# define participant codes
part <- paste("part",seq(1:N),sep="_")

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0, 0), Sigma=rho)
  data[,1] = data[,1]
  data[,2] = data[,2]+d1
  data[,3] = data[,3]
  data[,4] = data[,4]+d2
  datalong = c(data[,1],data[,2],data[,3],data[,4])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 4))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/(fac1*fac2)), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:fac1:fac2'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- t.test(data[,1],data[,2], paired=TRUE)$p.value
  p3[i] <- t.test(data[,3],data[,4], paired=TRUE)$p.value
}
close(pb)#close progress bar

# results are as predicted when the interaction is significant
supportH1 <- sum((p1<alpha1) & (p2<alpha1) & (p3 >= alpha1))/nSim
cat("Power of the test is ",supportH1,"\n")

signfac1 <- sum(p2<alpha1)/nSim
cat("Significance factor 1 level A ",signfac1,"\n")

signfac2 <- sum(p3<alpha1)/nSim
cat("Significance factor 1 level B ",signfac2,"\n")

signint <- sum(p1<alpha1)/nSim
cat("Significance interaction ",signint,"\n")


# 4.
# Simulations to estimate the power of a 2x2 repeated-measures ANOVA 
# Interaction: factor 1 has effect size of d = .4 at level 1 of factor 2 and d = .0 at the second level
# correlation = .9 and dav adapted so that dz = .4 (see below for other situations)
# we use the built-in aov-test command

# give sample size
N = 110

# give effect size d
dz1 = .4 #half of the effect of the first variable 
dz2 = .0 #half of the effect of the second variable

# give the correlation between the conditions
r = .9

# calculate dav
dav1 = dz1*sqrt(2*(1-r))
dav2 = dz2*sqrt(2*(1-r))

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value factor 1
p2 <-numeric(nSim) #p-value factor 2
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r, r), c(r, 1, r, r), c(r, r, 1, r), c(r, r, r, 1))

# define participant codes
part <- paste("part",seq(1:N),sep="_")

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0, 0), Sigma=rho)
  data[,1] = data[,1]
  data[,2] = data[,2]+dav1
  data[,3] = data[,3]
  data[,4] = data[,4]+dav2
  datalong = c(data[,1],data[,2],data[,3],data[,4])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 4))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/(fac1*fac2)), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:fac1:fac2'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- t.test(data[,1],data[,2], paired=TRUE)$p.value
  p3[i] <- t.test(data[,3],data[,4], paired=TRUE)$p.value
}
close(pb)#close progress bar

# results are as predicted when the interaction is significant
supportH1 <- sum((p1<alpha1) & (p2<alpha1) & (p3 >= alpha1))/nSim
cat("Power of the test is ",supportH1,"\n")

signfac1 <- sum(p2<alpha1)/nSim
cat("Significance factor 1 level A ",signfac1,"\n")

signfac2 <- sum(p3<alpha1)/nSim
cat("Significance factor 1 level B ",signfac2,"\n")

signint <- sum(p1<alpha1)/nSim
cat("Significance interaction ",signint,"\n")





# 5.
# Simulations to estimate the power of a 2x2 repeated-measures ANOVA 
# Interaction: factor 1 has effect size of d = .8 at level 1 of factor 2 and d = .4 at the second level
# correlation = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command

# give sample size
N = 105

# give effect size d
d1 = .8 #half of the effect of the first variable 
d2 = .4 #half of the effect of the second variable

# give the correlation between the conditions
r = .5

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value factor 1
p2 <-numeric(nSim) #p-value factor 2
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r, r), c(r, 1, r, r), c(r, r, 1, r), c(r, r, r, 1))

# define participant codes
part <- paste("part",seq(1:N),sep="_")

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0, 0), Sigma=rho)
  data[,1] = data[,1]
  data[,2] = data[,2]+d1
  data[,3] = data[,3]
  data[,4] = data[,4]+d2
  datalong = c(data[,1],data[,2],data[,3],data[,4])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 4))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/(fac1*fac2)), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:fac1:fac2'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- t.test(data[,1],data[,2], paired=TRUE)$p.value
  p3[i] <- t.test(data[,3],data[,4], paired=TRUE)$p.value
}
close(pb)#close progress bar

# results are as predicted when the interaction is significant
supportH1 <- sum((p1<alpha1) & (p2<alpha1) & (p3 < alpha1))/nSim
cat("Power of the test is ",supportH1,"\n")

signfac1 <- sum(p2<alpha1)/nSim
cat("Significance factor 1 level A ",signfac1,"\n")

signfac2 <- sum(p3<alpha1)/nSim
cat("Significance factor 1 level B ",signfac2,"\n")

signint <- sum(p1<alpha1)/nSim
cat("Significance interaction ",signint,"\n")


# 6
# Simulations to estimate the power of a 2x2 repeated-measures ANOVA 
# Interaction: factor 1 has effect size of d = .4 at level 1 of factor 2 and d = .8 at the second level
# correlation = .9 and dav adapted so that dz = .4 and .8 (see below for other situations)
# we use the built-in aov-test command

# give sample size
N = 105

# give effect size d
dz1 = .4 #half of the effect of the first variable 
dz2 = .8 #half of the effect of the second variable

# give the correlation between the conditions
r = .9

# calculate dav
dav1 = dz1*sqrt(2*(1-r))
dav2 = dz2*sqrt(2*(1-r))

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value factor 1
p2 <-numeric(nSim) #p-value factor 2
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r, r, r), c(r, 1, r, r), c(r, r, 1, r), c(r, r, r, 1))

# define participant codes
part <- paste("part",seq(1:N),sep="_")

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data = mvrnorm(n=N, mu=c(0, 0, 0, 0), Sigma=rho)
  data[,1] = data[,1]
  data[,2] = data[,2]+dav1
  data[,3] = data[,3]
  data[,4] = data[,4]+dav2
  datalong = c(data[,1],data[,2],data[,3],data[,4])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 4))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/(fac1*fac2)), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID:fac1:fac2'[[1]]$'Pr(>F)'[[1]]
  p2[i] <- t.test(data[,1],data[,2], paired=TRUE)$p.value
  p3[i] <- t.test(data[,3],data[,4], paired=TRUE)$p.value
}
close(pb)#close progress bar

# results are as predicted when the interaction is significant
supportH1 <- sum((p1<alpha1) & (p2<alpha1) & (p3 < alpha1))/nSim
cat("Power of the test is ",supportH1,"\n")

signfac1 <- sum(p2<alpha1)/nSim
cat("Significance factor 1 level A ",signfac1,"\n")

signfac2 <- sum(p3<alpha1)/nSim
cat("Significance factor 1 level B ",signfac2,"\n")

signint <- sum(p1<alpha1)/nSim
cat("Significance interaction ",signint,"\n")

