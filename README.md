
Overview
--------

THIS IS AN UNTESTED ALPHA VERSION WITH KNOWN ERRORS - ONLY USE FOR TESTING PURPOSES

This code allows you to simulate factorial ANOVA's and calculate the power of main effects, interactions, and follow up contrasts.

It is work in progress. Expect many bugs.

Installation
------------

``` r
# Install the two functions from GitHub by running the code below:

source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_design.R")
source("https://raw.githubusercontent.com/Lakens/ANOVA_power_simulation/master/ANOVA_power.R")
```

As you can see in your R environment, you will now have two functions, ANOVA\_design and ANOVA\_power. ANOVA\_design can be used to specify the design of the study you want to simulate. ANOVA\_power then uses the result from the ANOVA\_design function to simulate studies and return the power (based on simulations) for the ANOVA (main effects and interactions) and the contrasts.

The ANOVA\_design function
--------------------------

Currently the ANOVA\_design function can create designs up three factors, for both within, between, and mixed designs. It requires the following input: string, n, mu, sd, r, and p\_adjust.

1.  string: string that specifies the design (see below)
2.  n: the sample size for each between subject condition
3.  mu: a vector with the means for each condition
4.  sd: the population standard deviation. Currenlty assumes homogeneity of variances (only one sd can be provided)
5.  r: the correlation for within designs (or 0 for between designs)
6.  p\_adjust: adjustment method for multiple comparisons. E.g., "none" or "holm".

### specifying the design using string

"string" is used to specify the design. Add numbers for each factor that specify the number of levels in the factors (e.g., 2 for a factor with 2 levels). Add a "w"" after the number for within factors, and a "b"" for between factors. Seperate factors with a \* (asteriks). Thus "2b\*3w" is a design with two factors, the first of which has 2 between levels, and the second of which has 3 within levels.

### specifying the means using mu

Note that for each cell in the design, a mean must be provided. Thus, for a "2b\*3w" design, 6 means need to be entered.

Means need to be entered in the correct order. ANOVA\_design outputs a plot so you can check if you entered means correctly.

The general principle is that the code generates factors, indicated by letters of the alphabet, (i.e., a, b, and c). Levels are indicated by numbers (e.g., a1, a2, a3, etc). Means are entered in the following order for a 3 factors design:

1.  a1 b1 c1
2.  a1 b1 c2
3.  a1 b2 c1
4.  a1 b2 c2
5.  a2 b1 c1
6.  a2 b1 c2
7.  a2 b2 c1
8.  a2 b2 c2

The plot below vizualizes means from 1 to 8 being entered in a vector: mu = c(1, 2, 3, 4, 5, 6, 7, 8).

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

The ANOVA\_power function
-------------------------

The ANOVA\_power function takes the result from the ANOVA\_design function, and simulates data nsims times. As output, it provides a table for the ANOVA results, and the results for contrasts.

At least on windows systems, a progress bar should appear that shows the progress for the simulation. Larger numbers yield more accurate results, but also take a long time. I recommend testing with 100 simulations, or 1000 if you are getting a coffee.

An Example
----------

In the example below, 100 simulations for a 2\*2 mixed design (first factor between, second factor within) is performed. The sample size is 40 in each between subject condition (so 80 participants in total), the sd is 1.03, the correlation for the within factors is 0.87, and the means are 1.03, 1.21, 0.98, 1.01. No correction for multiple comparisons is made.

``` r
design_result <- ANOVA_design(string = "2b*2w",
                   n = 40, 
                   mu = c(1.03, 1.21, 0.98, 1.01), 
                   sd = 1.03, 
                   r=0.87, 
                   p_adjust = "none")
```

    ## Contrasts set to contr.sum for the following variables: a

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
ANOVA_power(design_result, nsims = 100)
```

    ## Contrasts set to contr.sum for the following variables: a

    ## No id variables; using all as measure variables

    ## Power and Effect sizes for ANOVA tests
    ##             power effect size
    ## anova_p_a      15       0.019
    ## anova_p_b      98       0.165
    ## anova_p_a:b    78       0.093
    ## 
    ## Power and Effect sizes for contrasts
    ##                                   power effect size
    ## paired_comparison_p_a1,b1 - a2,b1    10        0.06
    ## paired_comparison_p_a1,b1 - a1,b2   100       -0.74
    ## paired_comparison_p_a1,b1 - a2,b2     6        0.03
    ## paired_comparison_p_a2,b1 - a1,b2    25       -0.18
    ## paired_comparison_p_a2,b1 - a2,b2    13       -0.13
    ## paired_comparison_p_a1,b2 - a2,b2    20        0.16

The result for the power simulation reveal power is very high for the main effect of b - remember that this is the within-subjects factor, and the means are highly correlated (0.87) - so we have high power for within comparisons. This is also clear from the contrasts, where power is very high for the a1,b1-a1,b2 contrast (the within-subject contrast where means differ).

Power is very low for the minor differences among the three similar means (1.03, 0.98, 1.01) and the main effect of a (between-subjects factor). Power is reasonable but not high for the within-between interaction.
