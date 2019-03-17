
Validation of Power in Mixed ANOVA
----------------------------------

We install the functions:

``` r
# Install the two functions from GitHub by running the code below:

source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_design.R")
source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_power_ttest.R")
```

Two by two ANOVA, within-between design
---------------------------------------

We can simulate a Two-Way ANOVA with a specific alpha, sample size and effect size, to achieve a specified statistical power.

``` r
mu_from_ES <- function(K, ES){ # provides the vector of population means for a given population ES and nr of groups
  f2 <- ES/(1-ES)
  if(K == 2){
    a <- sqrt(f2)
    muvec <- c(-a,a)
  }
  if(K == 3){
    a <- sqrt(3*f2/2)
    muvec <- c(-a, 0, a)
  }
  if(K == 4){
    a <- sqrt(f2)
    muvec <- c(-a, -a, a, a)
  } # note: function gives error when K not 2,3,4. But we don't need other K.
  return(muvec)
}
```

``` r
mu <- c(-0.25, 0.25, 0.25, -0.25)
n <- 23
sd <- 1
r <- 0.5
string = "2w*2b"
alpha_level <- 0.05
p_adjust = "none"
labelnames = c("age", "old", "young", "color", "blue", "red")
design_result <- ANOVA_design(string = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r, 
                              p_adjust = p_adjust,
                              labelnames = labelnames)
```

![](validation_power_between_within_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
simulation_result <- ANOVA_power(design_result, alpha = 0.05, nsims = nsims)
```

    ## Power and Effect sizes for ANOVA tests
    ##                 power effect size
    ## anova_color      5.05      0.0101
    ## anova_age        5.28      0.0104
    ## anova_color:age 91.40      0.2085
    ## 
    ## Power and Effect sizes for contrasts
    ##                                            power effect size
    ## p_age_old_color_blue_age_old_color_red     38.05      0.5115
    ## p_age_old_color_blue_age_young_color_blue  62.38      0.5171
    ## p_age_old_color_blue_age_young_color_red    5.15      0.0008
    ## p_age_old_color_red_age_young_color_blue    4.85     -0.0033
    ## p_age_old_color_red_age_young_color_red    63.47     -0.5184
    ## p_age_young_color_blue_age_young_color_red 38.08     -0.5068
