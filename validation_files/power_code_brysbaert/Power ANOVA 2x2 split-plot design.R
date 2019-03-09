# 1.
# Simulations to estimate the power of a 2x2 split-plot ANOVA 
# the effect of repeated measures variable is set to dz = .4; 
# the between-groups variable is a control variable that is not assumed to have an effect
# correlation repeated measure = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command

# give sample size (per group)
N = 27

# give effect size d
dz1 = .4 #difference of the repeated measure variable 
dz2 = .0 #difference of the between-groups variable, also no interaction

# give the correlation between the repeated measures conditions
r = .5

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
p1 <-numeric(nSim) #p-value factor 1 (between-groups)
p2 <-numeric(nSim) #p-value factor 2 (repeate measure)
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r), c(r, 1))

# define participant codes
N2 = N*2
part <- paste("part",seq(1:N2),sep="_")
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data1 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data1[,2] = data1[,2]+dav1
  data2 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data2[,1] = data2[,1]+dav2
  data2[,2] = data2[,2]+dav1+dav2
  datalong = c(data1[,1],data2[,1],data1[,2],data2[,2])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 2))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/fac2), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID'[[1]]$'Pr(>F)'[[1]] #between
  p2[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[1]] #within
  p3[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[2]] #interaction
  }
close(pb)#close progress bar

# results are as predicted when main effect of repeated measures variable is significant
supportH1 <- sum(p2<alpha1)/nSim
cat("Power of the repeated-meausres test is ",supportH1,"\n")

signfac2 <- sum(p1<alpha1)/nSim
cat("Significance between-groups ",signfac2,"\n")

signint <- sum(p3<alpha1)/nSim
cat("Significance interaction ",signint,"\n")

hist(p2, breaks=20)





# 2.
# Simulations to estimate the power of a 2x2 split-plot ANOVA 
# the effect of repeated measure is set to dz = .4; the between-groups variable is a control variable that is not assumed to have an effect
# correlation = .9 and dav is decreased so that dz = .4 (see below for other situations)
# we use the built-in aov-test command


# give sample size (per group)
N = 27

# give effect size d
dz1 = .4 #difference of the repeated measure variable 
dz2 = .0 #difference of the between-groups variable, also no interaction

# give the correlation between the repeated measures conditions
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
p1 <-numeric(nSim) #p-value factor 1 (between-groups)
p2 <-numeric(nSim) #p-value factor 2 (repeate measure)
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r), c(r, 1))

# define participant codes
N2 = N*2
part <- paste("part",seq(1:N2),sep="_")
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data1 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data1[,2] = data1[,2]+dav1
  data2 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data2[,1] = data2[,1]+dav2
  data2[,2] = data2[,2]+dav1+dav2
  datalong = c(data1[,1],data2[,1],data1[,2],data2[,2])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 2))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/fac2), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID'[[1]]$'Pr(>F)'[[1]] #between
  p2[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[1]] #within
  p3[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[2]] #interaction
}
close(pb)#close progress bar

# results are as predicted when main effect of first repeated measures variable is significant
supportH1 <- sum(p2<alpha1)/nSim
cat("Power of the repeated-meausres test is ",supportH1,"\n")

signfac2 <- sum(p1<alpha1)/nSim
cat("Significance between-groups ",signfac2,"\n")

signint <- sum(p3<alpha1)/nSim
cat("Significance interaction ",signint,"\n")

hist(p2, breaks=20)



# 3.
# Simulations to estimate the power of a 2x2 split-plot ANOVA 
# Interaction: repeated measure has effect size of d = .4, but groups are part of a Latin-square design with a similar difference between both stimulus sets
# correlation = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command

# give sample size (per group)
N = 27

# give effect size d
dz1 = .4 #difference of the repeated measure variable 
dz2 = .4 #difference due to the stimulus set in the Latin-square design

# give the correlation between the repeated measures conditions
r = .5

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
p1 <-numeric(nSim) #p-value factor 1 (between-groups)
p2 <-numeric(nSim) #p-value factor 2 (repeate measure)
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r), c(r, 1))

# define participant codes
N2 = N*2
part <- paste("part",seq(1:N2),sep="_")
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data1 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data1[,2] = data1[,2]+dav1+dav2
  data2 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data2[,1] = data2[,1]+dav2
  data2[,2] = data2[,2]+dav1
  datalong = c(data1[,1],data2[,1],data1[,2],data2[,2])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 2))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/fac2), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID'[[1]]$'Pr(>F)'[[1]] #between
  p2[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[1]] #within
  p3[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[2]] #interaction
}
close(pb)#close progress bar

# results are as predicted when main effect of first repeated measures variable is significant
supportH1 <- sum(p2<alpha1)/nSim
cat("Power of the repeated-meausres test is ",supportH1,"\n")

signfac2 <- sum(p1<alpha1)/nSim
cat("Significance between-groups ",signfac2,"\n")

signint <- sum(p3<alpha1)/nSim
cat("Significance interaction ",signint,"\n")

hist(p2, breaks=20)





# 4.
# Simulations to estimate the power of a 2x2 split-plot ANOVA 
# the effect of between groups variable is set to dz = .4; 
# the repeated measures variable is not assumed to have an effect
# correlation repeated measure = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command

# give sample size (per group)
N = 95

# give effect size d
dz1 = .0 #difference of the repeated measure variable 
dz2 = .4 #difference of the between-groups variable, no interaction

# give the correlation between the repeated measures conditions
r = .9

# calculate dav
dav1 = dz1*sqrt(2*(1-r))
dav2 = dz2

# give number of simulations
nSim = 5000

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA

# create progress bar in case it takes a while
pb <- winProgressBar(title = "progress bar", min = 0, max = nSim, width = 300)

# create vectors to store p-values
p1 <-numeric(nSim) #p-value factor 1 (between-groups)
p2 <-numeric(nSim) #p-value factor 2 (repeate measure)
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r), c(r, 1))

# define participant codes
N2 = N*2
part <- paste("part",seq(1:N2),sep="_")
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data1 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data1[,2] = data1[,2]+dav1
  data2 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data2[,1] = data2[,1]+dav2
  data2[,2] = data2[,2]+dav1+dav2
  datalong = c(data1[,1],data2[,1],data1[,2],data2[,2])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 2))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/fac2), data=output)
  tests <- (summary(test))
  p1[i] <- tests$'Error: partID'[[1]]$'Pr(>F)'[[1]] #between
  p2[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[1]] #within
  p3[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[2]] #interaction
}
close(pb)#close progress bar

# results are as predicted when main effect of between-groups variable is significant
supportH1 <- sum(p1<alpha1)/nSim
cat("Power of the between groups test is ",supportH1,"\n")

signfac2 <- sum(p2<alpha1)/nSim
cat("Significance repeated measures ",signfac2,"\n")

signint <- sum(p3<alpha1)/nSim
cat("Significance interaction ",signint,"\n")

hist(p1, breaks=20)




# 5.
# Simulations to estimate the power of a 2x2 split-plot ANOVA 
# Interaction: repeated measure has effect size of d = .4 at level 1 of between-groups variable and d = -.4 at the other level level
# correlation = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command


# give sample size (per group)
N = 66

# give effect size d
dz1 = .4 #difference of the repeated measure variable in first group
dz2 = -.4 #difference of the repeated measure variable in second group

# give the correlation between the repeated measures conditions
r = .5

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
p1 <-numeric(nSim) #p-value factor 1 (between-groups)
p2 <-numeric(nSim) #p-value factor 2 (repeate measure)
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r), c(r, 1))

# define participant codes
N2 = N*2
part <- paste("part",seq(1:N2),sep="_")
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data1 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data1[,1] = data1[,1]-dav1/2
  data1[,2] = data1[,2]+dav1/2
  data2 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data2[,1] = data2[,1]-dav2/2
  data2[,2] = data2[,2]+dav2/2
  datalong = c(data1[,1],data2[,1],data1[,2],data2[,2])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 2))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/fac2), data=output)
  tests <- (summary(test))
  p3[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[2]] #interaction
  p1[i] <- t.test(data1[,1],data1[,2], paired=TRUE)$p.value
  p2[i] <- t.test(data2[,1],data2[,2], paired=TRUE)$p.value
}
close(pb)#close progress bar

# results are as predicted when interaction is significant and Bonferroni corrected one-tailed t tests of the repated measures effect for each group
supportH1 <- sum((p3<alpha1) & (p1<alpha1) & (p2<alpha1))/nSim
cat("Power of the repeated-meausres test is ",supportH1,"\n")

signfac1 <- sum(p1<alpha1)/nSim
cat("Significance t-test first group ",signfac1,"\n")

signfac2 <- sum(p2<alpha1)/nSim
cat("Significance t-test first group ",signfac2,"\n")

signint <- sum(p3<alpha1)/nSim
cat("Significance interaction ",signint,"\n")

hist(p3, breaks=20)



# 6.
# Simulations to estimate the power of a 2x2 split-plot ANOVA 
# Interaction: repeated measure has effect size of d = .8 at level 1 of between-groups variable and d = .4 at the other level level
# correlation = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command


# give sample size (per group)
N = 110

# give effect size d
dz1 = .8 #difference of the repeated measure variable in first group
dz2 = .4 #difference of the repeated measure variable in second group

# give the correlation between the repeated measures conditions
r = .5

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
p1 <-numeric(nSim) #p-value factor 1 (between-groups)
p2 <-numeric(nSim) #p-value factor 2 (repeate measure)
p3 <-numeric(nSim) #p-value interaction

# open library MASS to generate a matrix with correlated variables
library('MASS')

# define correlation matrix
rho <- cbind(c(1, r), c(r, 1))

# define participant codes
N2 = N*2
part <- paste("part",seq(1:N2),sep="_")
part

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  data1 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data1[,2] = data1[,2]+dav1
  data2 = mvrnorm(n=N, mu=c(0, 0), Sigma=rho)
  data2[,2] = data2[,2]+dav2
  datalong = c(data1[,1],data2[,1],data1[,2],data2[,2])
  fac1= factor(rep(letters[1:2], each = N, times=2))
  fac2= factor(rep(letters[3:4], each = N*2))
  partID = factor(rep(part, times = 2))
  output <-data.frame(partID,fac1,fac2,datalong)
  test <- aov(datalong~fac1*fac2 + Error(partID/fac2), data=output)
  tests <- (summary(test))
  p3[i] <- tests$'Error: partID:fac2'[[1]]$'Pr(>F)'[[2]] #interaction
  p1[i] <- t.test(data1[,1],data1[,2], paired=TRUE)$p.value
  p2[i] <- t.test(data2[,1],data2[,2], paired=TRUE)$p.value
}
close(pb)#close progress bar

# results are as predicted when main effect of first repeated measures variable is significant
supportH1 <- sum((p3<alpha1) & (p1<alpha1) & (p2<alpha1))/nSim
cat("Power of the repeated-meausres test is ",supportH1,"\n")

signfac1 <- sum(p1<alpha1)/nSim
cat("Significance t-test first group ",signfac1,"\n")

signfac2 <- sum(p2<alpha1)/nSim
cat("Significance t-test second group ",signfac2,"\n")

signint <- sum(p3<alpha1)/nSim
cat("Significance interaction ",signint,"\n")

hist(p3, breaks=20)




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

