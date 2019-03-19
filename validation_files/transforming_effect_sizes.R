# f from eta-squared
eta <- 0.05882353
f <- sqrt(eta/(1-eta))

# eta-squared from f
f<-0.25
eta <- f^2/(f^2+1)

# d from eta
# for between design
d <- 2*sqrt(eta/(1-eta))
# for within design
d <- sqrt(eta/(1-eta))


# f from d
# the within function works for between when r is set to 0.5, because d and dz are then equal.
# for between design
f <- 0.5*d
# for within design
r <- 0.8
d <- 0.5
f <- 0.5*d/sqrt(2*(1-r))

#from d to dz
d <- 0.5
r <- 0.7

dz <- d/(sqrt(2*(1-r)))
dz

#Formula used by G*Power for within design
f <- 0.25
k <- 1
m <- 4
n <- 10
e <- 1 #non-spericity correction
r <- 0.7
d <- 0.5
alpha <- 0.05

df1 <- (m - 1) * e
df2 <- (n - k) * (m - 1) * e

lambda <- (n * m * f^2) / (1 - r) # lambda within

F_critical <- qf(alpha,
                 df1,
                 df2, 
                 lower.tail=FALSE) # F critical within

pow <- pf(qf(alpha, #power 
             df1, 
             df2, 
             lower = FALSE), 
          df1, 
          df2, 
          lambda, 
          lower = FALSE)

#Formula used by G*Power for between design
f <- 0.25
k <- 2
m <- 1
n <- 10
e <- 1 #non-spericity correction
r <- 0.0
d <- 0.5
alpha <- 0.05

df1 <- (k - 1) * e
df2 <- (n * k - k) * e

lambda <- k * n * (m/(1 + (m - 1) * r)) * f^2 # lambda between
F_critical <- qf(alpha,
                 df1,
                 df2, 
                 lower.tail=FALSE) # F critical between

pow <- pf(F_critical, 
          df1, 
          df2, 
          lambda, 
          lower.tail = FALSE) # power between

# from mu and sd to f (Cohen, 275)
mu <- c(10, 13, 15.667)
sd <- 9
var_within <- sum((mu-mean(mu))^2)/length(mu) #Variance of the within effect (Cohen, 1988, formula 8.2.2)
var_group <- sd^2
f <- sqrt(sum((mu-mean(mu))^2)/length(mu))/sd #Cohen, 1988, formula 8.2.1

mu <- c(0, 0.4, 0.4)
sd <- 1
var_within <- sum((mu-mean(mu))^2)/length(mu) #Variance of the within effect
var_group <- sd^2
f <- sqrt(var_within)/sqrt(var_group)

mu <- c(0, 0.0, 0.4)
sd <- 1
var_within <- sum((mu-mean(mu))^2)/length(mu) #Variance of the within effect
var_group <- sd^2
f <- sqrt(var_within)/sqrt(var_group)
