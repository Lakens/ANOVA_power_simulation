
Overview
--------

THIS IS AN UNTESTED ALPHA VERSION - TAKE GREAT CARE IN USING THESE FUNCTIONS

This code allows you to simulate factorial ANOVA's and calculate the power of main effects, interactions, and follow up contrasts.

It is work in progress.

I've created some validation files that suggest the simulations work for [One-Way ANOVA designs](https://github.com/Lakens/ANOVA_power_simulation/blob/master/validation_files/validation_power_between.md) and one factor [within ANOVA designs](https://github.com/Lakens/ANOVA_power_simulation/blob/master/validation_files/validation_power_within.md). More validation work is needed.

Installation functions and packages
-----------------------------------

Run the code below to download the functions, and load the packages. Make sure to install these packages if needed.

``` r
# Install the two functions from GitHub by running the code below:
# You need to be connected to the internet.

source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_design.R")
source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_power.R")

###############
# Load libraries ----
###############

library(mvtnorm)
#Developmental version of afex is needed for now
devtools::install_github("singmann/afex@master") 
library(afex)
library(emmeans)
library(ggplot2)
library(gridExtra)
library(reshape2)
```

As you can see in your R environment, you will now have two functions, ANOVA\_design and ANOVA\_power. *ANOVA\_design* can be used to specify the design of the study you want to simulate. *ANOVA\_power* then uses the result from the ANOVA\_design function to simulate studies and return the power (based on simulations) for the ANOVA (main effects and interactions) and the contrasts.

The ANOVA\_design function
--------------------------

Currently the ANOVA\_design function can create designs up three factors, for both within, between, and mixed designs. It requires the following input: string, n, mu, sd, r, and p\_adjust, and labelnames.

1.  string: string that specifies the design (see below).
2.  n: the sample size for each between subject condition.
3.  mu: a vector with the means for each condition.
4.  sd: the population standard deviation. Currenlty assumes homogeneity of variances (only one sd can be provided).
5.  r: the correlation for within designs (or 0 for between designs).
6.  p\_adjust: adjustment method for multiple comparisons. E.g., "none" or "holm".
7.  labelnames: This is a number of words that indicate factor names and level names (see below).

### specifying the design using string

"string" is used to specify the design. Add numbers for each factor that specify the number of levels in the factors (e.g., 2 for a factor with 2 levels). Add a "w"" after the number for within factors, and a "b"" for between factors. Seperate factors with a \* (asteriks). Thus "2b\*3w" is a design with two factors (a 2b factor and a 3w factor), the first of which has 2 between levels (2b), and the second of which has 3 within levels (3w). An example of such a design is a group of people in one condition who get a drug, and a group of people in another condition who get a placebo, and we measure their health before they take the pill, one day after they take the pill, and a week after they take the pill.

### specifying the means using mu

Note that for each cell in the design, a mean must be provided. Thus, for a "2b\*3w" design, 6 means need to be entered.

Means need to be entered in the correct order. ANOVA\_design outputs a plot so you can check if you entered all means as you intended. Always carefully check if the plot that is generated matches your expectations.

The general principle is that the code generates factors, indicated by the factor names you entered in the labelnames variable, (i.e., *condition* and *time*). Levels are indicated by factor names and levels (e.g., control\_time1, control\_time2, control\_time3, etc).

If your design ha just one factor, just enter the means in the same order as the labelnames (see below). For more factors, note the general pattern in the example below. Means are entered in the following order for a 3 factors design (each with 2 levels):

1.  a1 b1 c1
2.  a1 b1 c2
3.  a1 b2 c1
4.  a1 b2 c2
5.  a2 b1 c1
6.  a2 b1 c2
7.  a2 b2 c1
8.  a2 b2 c2

So if you enter the means 1, 2, 3, 4, 5, 6, 7, 8 the first 4 means correspond to level 1 of factor 1, the second 4 means correspond to level 2 of factor 1. Within the first 4 means, the first 2 correspond to level 1 of factor 2, and within those 2 means, the first corresponds to level 1 of factor 3.

The plot below visualizes means from 1 to 8 being entered in a vector: mu = c(1, 2, 3, 4, 5, 6, 7, 8) so you can see how the basic ordering works.

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

### specifying label names

To make sure the plots and tables with simulation results are easy to interpret, it really helps to name all factors and levels. You can enter the labels in the 'labelnames' variable.

For the 2x3 design we have been using as an example, where there are 2 factors (condition and time of measurement), the first with 2 levels (placebo vs. medicine) and the second with three levels (time1, time2, and time3) we would enter the labels as follows:

c("condition", "placebo", "medicine", "time", "time1", "time2", "time3")

As you can see, you follow the order of the design (2b\*3w), and first write the FACTOR label (condition) followed by the levels of that factor (placebo and medicine). Then you write the second factor name (time) followed by the three labels for each level (time1, time2, time3). Do not use spaces in the names (so not "time 1" but "time1" or "time\_1").

The ANOVA\_power function
-------------------------

The ANOVA\_power function takes the result from the ANOVA\_design function, and simulates data nsims times using a specified alpha level. As output, it provides a table for the ANOVA results, and the results for all simple contrasts.

Simulations typically take some time. Larger numbers of simulations yield more accurate results, but also take a long time. I recommend testing the set up with 10 simulations, and run 1000 if the set-up is correct (or 10000 if you are getting a coffee).

An Example
----------

In the example below, 1000 simulations for a 2\*2 mixed design (first factor between, second factor within) is performed. The sample size is 80 in each between subject condition (so 160 participants in total), the sd is 1.03, the correlation for the within factors is 0.87, and the means are 1.03, 1.21, 0.98, 1.01. No correction for multiple comparisons is made.

The alpha level used as a significance threshold can be specified, and is set to 0.05 for this simulation.

``` r
design_result <- ANOVA_design(string = "2b*2w",
                   n = 80, 
                   mu = c(1.03, 1.21, 0.98, 1.01), 
                   sd = 1.03, 
                   r=0.87, 
                   p_adjust = "none",
                   labelnames = c("age", "old", "young", "speed", "fast", "slow"))
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
set.seed(1) #set seed to get reproducible results
simulation_result <- ANOVA_power(design_result, alpha = 0.05, nsims = 1000)
```

    ## Power and Effect sizes for ANOVA tests
    ##                 power effect size
    ## anova_age        18.7       0.008
    ## anova_speed      19.0       0.007
    ## anova_age:speed  10.9       0.005
    ## 
    ## Power and Effect sizes for contrasts
    ##                                                               power
    ## paired_comparison_age_old,speed_fast - age_young,speed_fast     5.4
    ## paired_comparison_age_old,speed_fast - age_old,speed_slow      21.0
    ## paired_comparison_age_old,speed_fast - age_young,speed_slow     4.5
    ## paired_comparison_age_young,speed_fast - age_old,speed_slow    32.5
    ## paired_comparison_age_young,speed_fast - age_young,speed_slow   5.9
    ## paired_comparison_age_old,speed_slow - age_young,speed_slow    25.5
    ##                                                               effect size
    ## paired_comparison_age_old,speed_fast - age_young,speed_fast          0.04
    ## paired_comparison_age_old,speed_fast - age_old,speed_slow           -0.19
    ## paired_comparison_age_old,speed_fast - age_young,speed_slow          0.01
    ## paired_comparison_age_young,speed_fast - age_old,speed_slow         -0.23
    ## paired_comparison_age_young,speed_fast - age_young,speed_slow       -0.03
    ## paired_comparison_age_old,speed_slow - age_young,speed_slow          0.20

The result for the power simulation has two sections. The first table provides power (from 0 to 100%) and effect sizes (partial eta-squared) for the ANOVA result. We see the results for the main effects of factor a, b and the interaction between a and b.

The result for the power simulation reveal power is very high for the main effect of b - remember that this is the within-subjects factor, and the means are highly correlated (0.87) - so we have high power for within comparisons. Power is lower for the interaction.

An ANOVA is typically followed up with contrasts. A statistical hypothesis often predicts not just an interaction, but also the shape of an interaction. For example, when looking at the plot of our design above, we might be specifically interested in comparing the mean in condition a1,b2 against a1,b1 and a2,b2 in simple contrasts.

The second table provides the power for all contrasts, and the effect sizes. Effect sizes are provided in Cohen's d for between-subject contrasts, and in Cohen's dz for within-subject contrasts (see Lakens, 2013). These are the effect sizes used in a-priori power analysis. Note that Cohen's d is slightly upwardly biased when calculated from observed data (as in these simulations).

Power is relatively high for the contrast comparing a1,b2-a1,b1 - remember this is the within-subject contrast where means differ, and the correlation between dependent observations is large (r = 0.87). Power for the contrast a1,b2-a2,b2 is much lower, because this is a between subjects comparison.

Power is very low for the minor differences among the three similar means (1.03, 0.98, 1.01) as can be seen from first, third, and fifth lines in the contrast output.

Note the difference in the effect size estimates. For the contrast a1,b1 - a1,b2 Cohen's dz is much larger (due to the strong positive correlation) than Cohen's d reported for the contrast a1,b2 - a2,b2, even though the raw mean differences are almost identical. This is because Cohen's dz takes the correlation into account.

In addition to the two tables, the ANOVA\_power function returns the raw simulation data (all p-values and effect sizes for each simulation, use simulation\_result$sim\_data) and a plot showing the p-value distributions for all tests in the ANOVA.

``` r
simulation_result$plot1
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)
