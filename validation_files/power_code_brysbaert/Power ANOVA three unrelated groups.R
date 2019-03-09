# Simulations to estimate the power of an ANOVA with three unrelated groups
# the effect between the two extreme groups is set to d = .4, the effect for the third group is d = .4 (see below for other situations)
# we use the built-in aov-test command

# give sample sizes (all samples sizes are equal)
N = 90

# give effect size d
d1 = .4 #difference between the extremes
d2 = .4 #third condition goes with the highest extreme

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
pes1 <-numeric(nSim) #partial eta-squared
pes2 <-numeric(nSim) #partial eta-squared two extreme conditions


library(lsr)

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  x<-rnorm(n = N, mean = 0, sd = 1)
  y<-rnorm(n = N, mean = d1, sd = 1) 
  z<-rnorm(n = N, mean = d2, sd = 1) 
  data = c(x,y,z)
  groups= factor(rep(letters[24:26], each = N))
  test <- aov(data~groups)
  pes1[i] <- etaSquared(test)[1,2]
  p1[i] <- summary(test)[[1]][["Pr(>F)"]][[1]]
  p2[i] <- t.test(x,y)$p.value
  p3[i] <- t.test(x,z)$p.value
  p4[i] <- t.test(y,z)$p.value
  data = c(x,y)
  groups= factor(rep(letters[24:25], each = N))
  test <- aov(data~groups)
  pes2[i] <- etaSquared(test)[1,2]
  }
close(pb)#close progress bar

# results are as predicted when omnibus ANOVA is significant, t-tests are significant between x and y plus x and z; not significant between y and z
supportH1 <- sum((p1<alpha1) & (p2<alpha2) & (p3<alpha2) & (p4>=alpha2))/nSim

supportOMNI <- sum(p1<alpha)/nSim

cat("Power of the test is ",supportH1,"\n")
cat("Power of ANOVA is", supportOMNI,"\n")
cat("Average partial eta squared complete ANOVA is", mean(pes1),"\n")
cat("Average partial eta squared two conditions is", mean(pes2),"\n")

hist(p1, breaks=20)



# Simulations to estimate the power of an ANOVA with three unrelated groups
# the effect between the two extreme groups is set to d = .4, the effect for the third group is d = .0
# we use the built-in aov-test command

# give sample sizes (all samples sizes are equal)
N = 145

# give effect size d
d1 = .4 #difference between the extremes
d2 = .0 #third condition goes with the lowest extreme

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

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  x<-rnorm(n = N, mean = 0, sd = 1)
  y<-rnorm(n = N, mean = d1, sd = 1) 
  z<-rnorm(n = N, mean = d2, sd = 1) 
  data = c(x,y,z)
  groups= factor(rep(letters[24:26], each = N))
  test <- aov(data~groups)
  p1[i] <- summary(test)[[1]][["Pr(>F)"]][[1]]
  p2[i] <- t.test(x,y)$p.value
  p3[i] <- t.test(x,z)$p.value
  p4[i] <- t.test(y,z)$p.value
}
close(pb)#close progress bar

# results are as predicted when omnibus ANOVA is significant, t-tests are significant between x and y plus y and z; not significant between x and z
supportH1 <- sum((p1<alpha1) & (p2<alpha2) & (p3>=alpha2) & (p4<alpha2))/nSim

supportOMNI <- sum(p1<alpha)/nSim

cat("Power of the test is ",supportH1,"\n")
cat("Power of ANOVA is", supportOMNI,"\n")

hist(p1, breaks=20)


# Simulations to estimate the power of an ANOVA with three unrelated groups
# the effect between the two extreme groups is set to d = .4, the effect for the third group is d = .2
# we use the built-in aov-test command

# give sample sizes (all samples sizes are equal)
N = 82

# give effect size d
d1 = .4 #difference between the extremes
d2 = .2 #third condition goes with the lowest extreme

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

for(i in 1:nSim){ #for each simulated experiment
  setWinProgressBar(pb, i, title=paste(round(i/nSim*100, 1), "% done"))
  x<-rnorm(n = N, mean = 0, sd = 1)
  y<-rnorm(n = N, mean = d1, sd = 1) 
  z<-rnorm(n = N, mean = d2, sd = 1) 
  data = c(x,y,z)
  groups= factor(rep(letters[24:26], each = N))
  test <- aov(data~groups)
  p1[i] <- summary(test)[[1]][["Pr(>F)"]][[1]]
  p2[i] <- t.test(x,y)$p.value
  p3[i] <- t.test(x,z)$p.value
  p4[i] <- t.test(y,z)$p.value
}
close(pb)#close progress bar

# results are as predicted when omnibus ANOVA is significant and all three post hoc t-tests are significant
supportH1 <- sum((p1<alpha1) & (p2<alpha2) & (p3<alpha2) & (p4<alpha2))/nSim

supportOMNI <- sum(p1<alpha)/nSim

cat("Power of the test is ",supportH1,"\n")
cat("Power of ANOVA is", supportOMNI,"\n")

hist(p1, breaks=20)

