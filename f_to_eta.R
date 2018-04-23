# I. Minimum variability: one mean at each end of d, the remaining k- 2 means all at the midpoint
mu = c(2, 0, 0, -2) # population means
sd = 1 #population standard deviations
k = length(mu)
d <- (max(mu)-min(mu))/sd
f <- .5*d
f2 <- f^2
ES <- f2/(f2+1)
ES


# 2. A pattern of medium variability results when the k means are equally spaced over the range, and therefore at intervals of d/(k- 1)
mu = c(6, 2, -2, -6) # population means
sd = 1 #population standard deviations
k = length(mu) 
d <- (max(mu)-min(mu))/sd
f <- (d/2)*sqrt((k+1)/(3*(k-1)))
f2 <- f^2
ES <- f2/(f2+1)
ES



# 3. Maximum variability: the means all at the end points of d
mu = c(-1, -1, 1, 1) # population means
sd = 1 #population standard deviations
k = length(mu)
d <- (max(mu)-min(mu))/sd
f <- ifelse(k %% 2 == 0, .5*d, d*(sqrt(k^2-1)/(2*k)))
f2 <- f^2
ES <- f2/(f2+1)
ES

