library(MASS)

design_result <- ANOVA_design(string = "2b*3w",
                              n = 80, 
                              mu = c(1.03, 1.21, 0.98, 1.01, 1, 1), 
                              sd = 1.03, 
                              r=0.87, 
                              p_adjust = "none",
                              labelnames = c("age", "old", "young", "speed", "fast", "slow", "medium"))

simulation_result <- ANOVA_power(design_result, alpha = 0.05, nsims = 100)


#simply repeated effect
design_result <- ANOVA_design(string = "2w",
                              n = 80, 
                              mu = c(1, 1.4), 
                              sd = 1, 
                              r=0.9, 
                              p_adjust = "none",
                              labelnames = c("age", "old", "young"))

simulation_result <- ANOVA_power(design_result, alpha = 0.05, nsims = 1000)

1/sqrt(2)*0.4

string = "2b*3w"
n = 20
mu = c(1.03, 1.21, 0.98, 1.01, 1, 1)
sd = 1.03
r=0.87
p_adjust = "none"
labelnames = c("age", "a1", "a2", "speed", "s1", "s2", "s3")


alpha <- 0.05
nsims <- 100

string = "2b*2w*2b"
n = 20
mu = c(1.03, 1.21, 0.98, 1.01, 1.11, 1.03, 1.22, 0.99)
sd = 1.03
r=0.87
p_adjust = "none"
labelnames = c("age", "a1", "a2", "speed", "s1", "s2", "time", "t1", "t2")


y <- c("age_*_speed_s1")
x <- glob2rx(c("age_a1_speed_s1", "age_a1_speed_s2", "age_a1_speed_s3", "age_a2_speed_s1", "age_a2_speed_s2", "age_a2_speed_s3"))
grepl(y, x)


x <- c("age_a1_speed_s1", "age_a1_speed_s2", "age_a1_speed_s3", "age_a2_speed_s1", "age_a2_speed_s2", "age_a2_speed_s3")



grepl("age_a1_speed_$", x)


y <- c("age_a1_speed_s1_time_*")
x <- c("age_a1_speed_s1_time_", "age_a1_speed_s2", "age_a1_speed_s3", "age_a2_speed_s1", "age_a2_speed_s2", "age_a2_speed_s3")
grepl(y, x)

