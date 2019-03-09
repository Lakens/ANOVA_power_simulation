# 1.
# Simulations to estimate the power of a 2x2 split-plot pre-post design 
# the effect of repeated measures variable is set to dz = .4 for one group and to dz = .0 for the other; 
# Instead of running a split-plot design, we run a between groups design in which we predict the second measurement
# on the basis of the first measurement and the group variable
# correlation repeated measure = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command

# give sample size (per group)
N = 80

# give effect size d
dz1 = .4 #difference of the repeated measure variable 
dz2 = .0 #difference of the between-groups variable,  no interaction

# give the correlation between the repeated measures conditions
r = .5

# calculate dav
dav1 = dz1*sqrt(2*(1-r))
dav2 = dz2 #dav = dz for between-groups design

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
  data2[,2] = data2[,2]+dav2
  datalong = rbind(data1,data2)
  fac1= factor(rep(letters[1:2], each = N))
  partID = factor(part)
  output <-data.frame(partID,fac1,datalong)
  test <- lm(X2~fac1+X1, data=output)
  tests <- (summary(test))
  p1[i] <- tests$coefficients[2,4][[1]]
  }
close(pb)#close progress bar

# results are as predicted when main effect of repeated measures variable is significant
supportH1 <- sum(p1<alpha1)/nSim
cat("Power of the between-groups test is ",supportH1,"\n")

hist(p1, breaks=20)

# 2.
# Simulations to estimate the power of a 2x2 split-plot pre-post design 
# the effect of repeated measures variable is set to dz = .4 for one group and to dz = .8 for the other; 
# Instead of running a split-plot design, we run a between groups design in which we predict the second measurement
# on the basis of the first measurement and the group variable
# correlation repeated measure = .5 so that dav = dz (see below for other situations)
# we use the built-in aov-test command

# give sample size (per group)
N = 80

# give effect size d
dz1 = .4 #difference of the repeated measure variable for group 1
dz2 = .8 #difference of the repeated measure variable for group 2

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
  datalong = rbind(data1,data2)
  fac1= factor(rep(letters[1:2], each = N))
  partID = factor(part)
  output <-data.frame(partID,fac1,datalong)
  test <- lm(X2~fac1+X1, data=output)
  tests <- (summary(test))
  p1[i] <- tests$coefficients[2,4][[1]]
}
close(pb)#close progress bar

# results are as predicted when main effect of repeated measures variable is significant
supportH1 <- sum(p1<alpha1)/nSim
cat("Power of the between-groups test is ",supportH1,"\n")

hist(p1, breaks=20)
