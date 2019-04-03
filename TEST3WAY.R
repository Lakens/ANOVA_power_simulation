data_mat <- matrix(c(23,	18,	26,	32,	13,	31,	26,	34,	17,	23,	28,	26,
24,	25,	14,	17,	30,	18,	11,	16,	25,	18,	14,	25,
16,	31,	17,	11,	34,	24,	24,	19,	31,	16,	11,	19,
31,	29,	40,	31,	35,	25,	18,	29,	36,	42,	40,	36,
19,	28,	31,	23,	19,	14,	4,	29,	18,	22,	18,	24,
29,	25,	17,	12,	26,	35,	10,	23,	26,	18,	24,	16,
28,	37,	22,	44,	37,	42,	30,	37,	25,	38,	41,	28,
35,	23,	24,	11,	23,	30,	26,	16,	23,	14,	19,	25), 8, 12)


  
  




#  From http://www.real-statistics.com/two-way-anova/anova-more-than-two-factors/
string <- "2b*2b*2b"
n <- 12
mu <- c(24.75, 19.75, 21.08333, 32.66667, 20.75, 21.75, 34.08333, 22.41667) 
sd <- 8.502418387 #calculated from the sample data
r <- 0.0
labelnames <- c("Gender", "male", "female", "Country", "Italian", "Foreign", 
                "Position", "Seated", "Prone") #

design_result <- ANOVA_design(string = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              labelnames = labelnames)

ANOVA_power(design_result, nsims = nsims)

power_analytic <- power_threeway_between(design_result)

power_analytic$power_A
power_analytic$power_B
power_analytic$power_C
power_analytic$power_AB
power_analytic$power_AC
power_analytic$power_BC
power_analytic$power_ABC

power_analytic$eta_p_2_A
power_analytic$eta_p_2_B
power_analytic$eta_p_2_C
power_analytic$eta_p_2_AB
power_analytic$eta_p_2_AC
power_analytic$eta_p_2_BC
power_analytic$eta_p_2_ABC


#ANOVA_power(design_result, nsims = 1000)

mu_array <- array(design_result$mu, dim = c(length(design_result$labelnames[[1]]),
                                            length(design_result$labelnames[[2]]),
                                            length(design_result$labelnames[[3]])))
#Gender
mu_A <- apply(mu_array,c(3),mean)
mu_A
#Country
mu_B <- apply(mu_array,c(2),mean)
mu_B
#Position
mu_C <- apply(mu_array,c(1),mean)
mu_C

#GENDER*NATIONALITY
mu_AB <- apply(mu_array,c(2,3),mean)
mu_AB
mu_AB <- mu_AB - (mean(design_result$mu) + sweep(mu_AB, 1, rowMeans(mu_AB)) + sweep(mu_AB, 2, colMeans(mu_AB)))
mu_AB

#SIZE*LOAD
mu_AC <- apply(mu_array,c(1,3),mean)
mu_AC
mu_AC <- mu_AC - (mean(design_result$mu) + sweep(mu_AC, 2, colMeans(mu_AC)) + sweep(mu_AC, 1, rowMeans(mu_AC)))
mu_AC

#COLOR*LOAD
mu_BC <- apply(mu_array,c(1,2),mean)
mu_BC
mu_BC <- mu_BC - (mean(design_result$mu) + sweep(mu_BC,1, rowMeans(mu_BC)) + sweep(mu_BC,2, colMeans(mu_BC)))
mu_BC

# Calculate degrees of freedom

df_A <- (length(design_result$labelnames[[1]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
df_B <- (length(design_result$labelnames[[2]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
df_C <- (length(design_result$labelnames[[3]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction

df_AB <- (length(design_result$labelnames[[1]]) - 1) * (length(design_result$labelnames[[2]]) - 1)
df_AC <- (length(design_result$labelnames[[1]]) - 1) * (length(design_result$labelnames[[3]]) - 1)
df_BC <- (length(design_result$labelnames[[2]]) - 1) * (length(design_result$labelnames[[3]]) - 1)

df_ABC <- (length(design_result$labelnames[[1]]) - 1) * (length(design_result$labelnames[[2]]) - 1) * (length(design_result$labelnames[[3]]) - 1)

df_error <- (n * length(design_result$mu)) - (length(design_result$labelnames[[1]])) * (length(design_result$labelnames[[2]])) * (length(design_result$labelnames[[3]]))
df_total <- df_error + df_A + df_B + df_C + df_AB + df_AC + df_BC + df_ABC

# Calculate sum of squares

MS_A <- design_result$n * length(design_result$labelnames[[2]]) * length(design_result$labelnames[[3]]) * (sum((mu_A - mean(mu_A))^2)/(length(design_result$labelnames[[2]])-1))
SS_A <- design_result$n * length(design_result$labelnames[[2]]) * length(design_result$labelnames[[3]]) * sum((mu_A - mean(mu_A))^2)

MS_B <- design_result$n * length(design_result$labelnames[[1]]) * length(design_result$labelnames[[3]]) * (sum((mu_B - mean(mu_B))^2)/(length(design_result$labelnames[[2]])-1))
SS_B <- design_result$n * length(design_result$labelnames[[1]]) * length(design_result$labelnames[[3]]) * sum((mu_B - mean(mu_B))^2)

MS_C <- design_result$n * length(design_result$labelnames[[1]]) * length(design_result$labelnames[[2]]) * (sum((mu_C - mean(mu_C))^2)/(length(design_result$labelnames[[2]])-1))
SS_C <- design_result$n * length(design_result$labelnames[[1]]) * length(design_result$labelnames[[2]]) * sum((mu_C - mean(mu_C))^2)

MS_AB <- design_result$n * length(design_result$labelnames[[3]]) * sum(mu_AB^2)/((length(design_result$labelnames[[1]])-1) * (length(design_result$labelnames[[2]])-1))
SS_AB <- design_result$n * length(design_result$labelnames[[3]]) * sum(mu_AB^2)

SS_AB_between <- design_result$n * length(design_result$labelnames[[3]]) * sum((apply(mu_array,c(2,3),mean) - mean(apply(mu_array,c(2,3),mean)))^2)
SS_AB_2 <- SS_AB_between - SS_A - SS_B 

MS_AC <- design_result$n * length(design_result$labelnames[[2]]) * sum(mu_AC^2)/((length(design_result$labelnames[[1]])-1) * (length(design_result$labelnames[[3]])-1))
SS_AC <- design_result$n * length(design_result$labelnames[[2]]) * sum(mu_AC^2)

SS_AC_between <- design_result$n * length(design_result$labelnames[[2]]) * sum((apply(mu_array,c(1,3),mean) - mean(apply(mu_array,c(1,3),mean)))^2)
SS_AC_2 <- SS_AC_between - SS_A - SS_C 

MS_BC <- design_result$n * length(design_result$labelnames[[1]]) * sum(mu_BC^2)/((length(design_result$labelnames[[2]])-1) * (length(design_result$labelnames[[3]])-1))
SS_BC <- design_result$n * length(design_result$labelnames[[1]]) * sum(mu_BC^2)

SS_BC_between <- design_result$n * length(design_result$labelnames[[1]]) * sum((apply(mu_array,c(1,2),mean) - mean(apply(mu_array,c(1,2),mean)))^2)
SS_BC_2 <- SS_BC_between - SS_B - SS_C 

MS_total <- design_result$sd^2
SS_total <- MS_error * df_total

SS_error <- SS_total - SS_A - SS_B - SS_C - SS_AB - SS_AC - SS_BC - SS_ABC

#Calculate eta-squared

eta_p_2_A <- SS_A/(SS_A+SS_error)
eta_p_2_A
eta_p_2_B <- SS_B/(SS_B+SS_error)
eta_p_2_B
eta_p_2_C <- SS_C/(SS_C+SS_error)
eta_p_2_C
eta_p_2_AB <- SS_AB/(SS_AB+SS_error)
eta_p_2_AB
eta_p_2_AC <- SS_AC/(SS_AC+SS_error)
eta_p_2_AC
eta_p_2_BC <- SS_BC/(SS_BC+SS_error)
eta_p_2_BC
eta_p_2_ABC <- SS_ABC/(SS_ABC+SS_error)
eta_p_2_ABC

