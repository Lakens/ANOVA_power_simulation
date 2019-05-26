library(tidyverse) # for data wrangling and visualisation
library(afex)      # for LMEM and ANOVA
library(faux)      # devtools::install_github("debruine/faux")
library(broom.mixed) # for extracting data from mixed effect models
set.seed(8675309)  # this makes sure your script uses the same set of random numbers each time you run the full script 
# (never set this inside a function or loop)
# 
iat_data <- readr::read_csv("iat_data.csv")
iat_data <- readr::read_csv("https://raw.githubusercontent.com/debruine/sim_mem/master/iat_data.csv")


agg_data <- iat_data %>%
  group_by(sub_id, condition) %>%
  summarise(rt = mean(rt)) %>%
  ungroup()

agg_data %>%
  ggplot(aes(condition, rt, fill = condition)) +
  geom_violin(trim = FALSE, show.legend = FALSE) +
  geom_boxplot(fill = "white", width = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c("red", "dodgerblue"))

t.test(rt~condition, agg_data, paired = TRUE)

vars <- agg_data %>%
  unite(var, condition) %>%
  spread(var, rt) %>%
  select(-sub_id)

sub_n <- nrow(vars)
m1    <- mean(vars$congruent)
m2    <- mean(vars$incongruent)
sd1   <- sd(vars$congruent)
sd2   <- sd(vars$incongruent)
r     <- cor(vars$congruent, vars$incongruent)

design_result_5 <- ANOVA_design(string = "2w", 
                                n = 20, 
                                mu = c(m1, m2), sd = mean(c(sd1, sd2)), 
                                r = r, 
                                labelnames = c("condition", "congruent", "incongruent"))
power_result_5 <- ANOVA_power(design_result_5, alpha_level = 0.01, p_adjust = "none", nsims = nsims, seed = 2019, verbose = TRUE)
