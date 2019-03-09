#This function allows you to calculate f, d and eta squared following Cohen, 1988, p 277
# From Cohen, 1988, p 277
# The patterns are:
# I. Minimum variability: one mean at each end of d, the remaining k- 2 means all at the midpoint.
# 2. Intermediate variability: the k means equally spaced over d.
# 3. Maximum variability: the means all at the end points of d.

# For each of these patterns, there is a fixed relationship between f and d for any given number of means, k.

# Pattern 1. For any given range of means, d, the minimum standard
# deviation, f1, results when the remaining k - 2 means are concentrated at
# the mean of the means (0 when expressed in standard units), i.e., half-way
# between the largest and smallest.
# 
# Pattern 2. A pattern of medium variability results when the k means
# are equally spaced over the range, and therefore at intervals of d/(k- 1).
# 
# Pattern 3. It is demonstrable and intuitively evident that for any given
# range the dispersion which yield~ the maximum standard deviation has the
# k means falling at both extremes of the range. When k is even, !k fall at
# - !d and the other !k fall at + !d; when k is odd, (k + I )/2 of the means
# fall at either end and the (k- 1)/2 remaining means at the other. With this
# pattern, for all even numbers of means, use formula (8.2.12).
# When k is odd, and there is thus one more mean at one extreme than at
# the other, use formula (8.2.13).

calc_f_d_eta <- function(mu, sd, variability){
  if(variability == "minimum"){
    k = length(mu)
    d <- (max(mu)-min(mu))/sd
    f <- .5*d
    f2 <- f^2
    ES <- f2/(f2+1)
    print(ES)
  }
  if(variability == "medium"){
    k = length(mu) 
    d <- (max(mu)-min(mu))/sd
    f <- (d/2)*sqrt((k+1)/(3*(k-1)))
    f2 <- f^2
    ES <- f2/(f2+1)
    print(ES)
  }
  if(variability == "maximum"){
    k = length(mu)
    d <- (max(mu)-min(mu))/sd
    f <- ifelse(k %% 2 == 0, .5*d, d*(sqrt(k^2-1)/(2*k)))
    f2 <- f^2
    ES <- f2/(f2+1)
    print(ES)
  }
  invisible(list(mu = mu,
                 sd = sd,
                 d = d,
                 f = f,
                 f2 = f2,
                 ES = ES))
}