
Validation of Power in Repeated Measures ANOVA
----------------------------------------------

We first repeat the simulation by Brysbaert:

``` r
# give sample size
N = 75

# give effect size d
d1 = .4 #difference between the extremes
d2 = .4 #third condition goes with the highest extreme

# give the correlation between the conditions
r = .5

# give number of simulations
nSim = nsims

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA
alpha2 = .05 #also adjusted from original by DL

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
```

    ## NULL

``` r
#printing all unique tests (adjusted code by DL)
sum(p1<alpha1)/nSim
```

    ## [1] 0.9545

``` r
sum(p2<alpha2)/nSim
```

    ## [1] 0.9273

``` r
sum(p3<alpha2)/nSim
```

    ## [1] 0.9287

``` r
sum(p4<alpha2)/nSim
```

    ## [1] 0.0497

Installation
------------

We install the functions:

``` r
# Install the two functions from GitHub by running the code below:

source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_design.R")
source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_power_ttest.R")
```

Reproducing Brysbaert
---------------------

We can reproduce the same results as Brysbaeert finds with his code:

``` r
string <- "3w"
n <- 75
mu <- c(0, 0.4, 0.4)
sd <- 1
r <- 0.5
p_adjust = "none"
labelnames <- c("speed", "fast", "medium", "slow")
```

We create the within design, and run the simulation

``` r
design_result <- ANOVA_design(string = string,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   r = r, 
                   p_adjust = "none",

                                      labelnames = labelnames)
```

![](validation_power_within_Brysbaert_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ANOVA_power(design_result, nsims = nsims)
```

    ## Power and Effect sizes for ANOVA tests
    ##             power effect size
    ## anova_speed    95      0.1033
    ## 
    ## Power and Effect sizes for contrasts
    ##                                                   power effect size
    ## p_speed_fast_speed_fast_speed_medium_speed_medium 92.65      0.4038
    ## p_speed_fast_speed_fast_speed_slow_speed_slow     92.48      0.4031
    ## p_speed_medium_speed_medium_speed_slow_speed_slow  5.37      0.0004

Results
=======

The results of the simulation are very similar. Power for the ANOVA *F*-test is around 95.2%. For the three paired t-tests, power is around 92.7. This is in line with the a-priori power analysis when using g\*power:

![](screenshots/gpower_2.png)

We can perform an post-hoc power analysis in G\*power. We can calculate Cohen´s f based on the means and sd, using our own custom formula.

``` r
# Install the function from GitHub by running the code below:

source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/calc_f_d_eta.R")

mu <- c(0, 0.4, 0.4)
res <- calc_f_d_eta(mu = mu, sd = 1, variability = "maximum")
res$f
```

    ## [1] 0.1885618

``` r
res$d
```

    ## [1] 0.4

``` r
res$ES
```

    ## [1] 0.03433476

The Cohen´s f is 0.1885618. Because the correlation is set to 0.5, we can enter the f (using the default 'as in G\*Power 3.0' in the option window) and enter a sample size of 75, number of groups as 1, number of measurements as 3, correlation as 0.5. This yields:

![](screenshots/gpower_3.png)

Reproducing Brysbaert Variation 1 Changing Correlation
------------------------------------------------------

We can reproduce the same results as Brysbaeert finds with his code:

We first repeat the simulation by Brysbaert:

``` r
# give sample size
N = 75

# give effect size d
d1 = .4 #difference between the extremes
d2 = .4 #third condition goes with the highest extreme

# give the correlation between the conditions
r = .6

# give number of simulations
nSim = nsims

# give alpha levels
alpha1 = .05 #alpha level for the omnibus ANOVA
alpha2 = .05 #also adjusted from original by DL

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
```

    ## NULL

``` r
#printing all unique tests (adjusted code by DL)
sum(p1<alpha1)/nSim
```

    ## [1] 0.9842

``` r
sum(p2<alpha2)/nSim
```

    ## [1] 0.9698

``` r
sum(p3<alpha2)/nSim
```

    ## [1] 0.9703

``` r
sum(p4<alpha2)/nSim
```

    ## [1] 0.0475

``` r
string <- "3w"
n <- 75
mu <- c(0, 0.4, 0.4)
sd <- 1
r <- 0.6
p_adjust = "none"
labelnames <- c("speed", "fast", "medium", "slow")
```

We create the within design, and run the simulation

``` r
design_result <- ANOVA_design(string = string,
                   n = n, 
                   mu = mu, 
                   sd = sd, 
                   r = r, 
                   p_adjust = "none",

                                      labelnames = labelnames)
```

![](validation_power_within_Brysbaert_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ANOVA_power(design_result, nsims = nsims)
```

    ## Power and Effect sizes for ANOVA tests
    ##             power effect size
    ## anova_speed  98.4       0.125
    ## 
    ## Power and Effect sizes for contrasts
    ##                                                   power effect size
    ## p_speed_fast_speed_fast_speed_medium_speed_medium 97.28      0.4544
    ## p_speed_fast_speed_fast_speed_slow_speed_slow     96.76      0.4527
    ## p_speed_medium_speed_medium_speed_slow_speed_slow  4.55     -0.0016

Again, this is similar to g\*power for the ANOVA:

![](screenshots/gpower_4.png)
