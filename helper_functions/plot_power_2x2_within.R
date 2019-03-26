power_2x2_within <- function(mu, m_A, m_B, sigma, n, rho_A, rho_B, rho_AB, alpha){
  library(ggplot2)
  library(gridExtra)
  library(mvtnorm)
  library(afex)
  library(emmeans)
  library(reshape2)
  library(pwr)
  
  
  mean_mat <- t(matrix(mu, 
                       nrow = 2,
                       ncol = 2)) #Create a mean matrix
  rownames(mean_mat) <- c("a1", "a2")
  colnames(mean_mat) <- c("b1", "b2")

  a1 <- mean_mat[1,1] - (mean(mean_mat) + (mean(mean_mat[1,]) - mean(mean_mat)) + (mean(mean_mat[,1]) - mean(mean_mat)))
  a2 <- mean_mat[1,2] - (mean(mean_mat) + (mean(mean_mat[1,]) - mean(mean_mat)) + (mean(mean_mat[,2]) - mean(mean_mat)))
  b1 <- mean_mat[2,1] - (mean(mean_mat) + (mean(mean_mat[2,]) - mean(mean_mat)) + (mean(mean_mat[,1]) - mean(mean_mat)))
  b2 <- mean_mat[2,2] - (mean(mean_mat) + (mean(mean_mat[2,]) - mean(mean_mat)) + (mean(mean_mat[,2]) - mean(mean_mat)))

  k <- 1 #one group (because all factors are within)

  m_A <- 2 #levels factor A
  variance_e_A <- sigma^2 * (1 - rho_A) + sigma^2 * (m_A - 1) * (rho_B - rho_AB) #Variance A

  m_B <- 2 #levels factor B
  variance_e_B <- sigma^2 * (1 - rho_B) + sigma^2 * (m_B - 1) * (rho_A - rho_AB) #Variance B

  variance_e_AB <- sigma^2 * (1 - max(rho_A, rho_B)) - sigma^2 * (min(rho_A, rho_B) - rho_AB) #Variance AB

  
  # For main effect A
  f_A <- sqrt(sum((rowMeans(mean_mat)-mean(rowMeans(mean_mat)))^2))/sigma
  lambda_A <- n * m_A * sum((rowMeans(mean_mat)-mean(rowMeans(mean_mat)))^2)/variance_e_A 
  df1 <- (m_A - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  df2 <- (n - k) * (m_A - 1) #calculate degrees of freedom 2
  F_critical_A <- qf(alpha, # critical F-vaue
                   df1,
                   df2, 
                   lower.tail=FALSE) 
  
  pow_A <- pf(qf(alpha, #power 
                 df1, 
                 df2, 
                 lower.tail = FALSE), 
              df1, 
              df2, 
              lambda_A, 
              lower.tail = FALSE)
  
  # For main effect B
  f_B <- sqrt(sum((colMeans(mean_mat)-mean(colMeans(mean_mat)))^2))/sigma
  lambda_B <- n * m_B * sum((colMeans(mean_mat)-mean(colMeans(mean_mat)))^2)/variance_e_B 
  df1 <- (m_B - 1) #calculate degrees of freedom 1
  df2 <- (n - k) * (m_B - 1) #calculate degrees of freedom 2
  F_critical_B <- qf(alpha, # critical F-vaue
                   df1,
                   df2, 
                   lower.tail=FALSE) 
  
  pow_B <- pf(qf(alpha, #power 
                 df1, 
                 df2, 
                 lower.tail = FALSE), 
              df1, 
              df2, 
              lambda_B, 
              lower.tail = FALSE)
  
  # For the interaction
  f_AB <- sqrt(sum(c(a1, a2, b1, b2)^2))/sigma #based on G*power manual page 28
  lambda_AB <- n * sqrt(sum(c(a1, a2, b1, b2)^2)/length(mu))/variance_e_AB 
  df1 <- (m_A - 1)*(m_B - 1)  #calculate degrees of freedom 1
  df2 <- (n - k) * (m_A - 1) * (m_B - 1) #calculate degrees of freedom 2
  F_critical_AB <- qf(alpha, # critical F-vaue
                   df1,
                   df2, 
                   lower.tail=FALSE) 
  
  pow_AB <- pf(qf(alpha, #power 
                  df1, 
                  df2, 
                  lower.tail = FALSE), 
               df1, 
               df2, 
               lambda_AB, 
               lower.tail = FALSE)
  
  
  
  
  invisible(list(mu = mu,
                 sigma = sigma,
                 n = n, 
                 rho_A = rho_A, 
                 rho_B = rho_B, 
                 rho_AB = rho_AB, 
                 alpha = alpha,
                 f_A = f_A,
                 f_B = f_B,
                 f_AB = f_AB,
                 lambda_A = lambda_A,
                 lambda_B = lambda_B,
                 lambda_AB = lambda_AB,
                 F_critical_A = F_critical_A,
                 F_critical_B = F_critical_B,
                 F_critical_AB = F_critical_AB,
                 pow_A = pow_A,
                 pow_B = pow_B,
                 pow_AB = pow_AB))
}

plot_power_2x2_within <- function(mu, m_A, m_B, sigma, rho_A, rho_B, rho_AB, alpha, max_n){
  n_vec <- seq(from = 2, to = max_n)
  
  power_A <- numeric(length(n_vec))
  power_B <- numeric(length(n_vec))
  power_AB <- numeric(length(n_vec))
  
  for (i in 1:length(n_vec)){
    power_res <- power_2x2_within(mu = mu, 
                                  m_A = m_A, 
                                  m_B = m_B, 
                                  sigma = sigma, 
                                  n = n_vec[i], 
                                  rho_A = rho_A, 
                                  rho_B = rho_B, 
                                  rho_AB = rho_AB, 
                                  alpha = alpha)
    power_A[i] <- power_res$pow_A*100
    power_B[i] <- power_res$pow_B*100
    power_AB[i] <- power_res$pow_AB*100
  }
  
  res_df <- data.frame(n_vec, power_A, power_B, power_AB)
  
  library(ggplot2)
  library(gridExtra)
  p1 <- ggplot(data=res_df, aes(x = n_vec, y = power_A)) +
    geom_line( size=1.5) +
    scale_x_continuous(limits = c(0, max(n_vec))) + 
    scale_y_continuous(limits = c(0, 100)) +
    theme_bw() +
    labs(x="Sample size", y = "Power Factor A")
  
  p2 <- ggplot(data=res_df, aes(x = n_vec, y = power_AB)) +
    geom_line( size=1.5) +
    scale_x_continuous(limits = c(0, max(n_vec))) + 
    scale_y_continuous(limits = c(0, 100)) +
    theme_bw() +
    labs(x="Sample size", y = "Power Factor B")
  
  p3 <- ggplot(data=res_df, aes(x = n_vec, y = power_AB)) +
    geom_line( size=1.5) +
    scale_x_continuous(limits = c(0, max(n_vec))) + 
    scale_y_continuous(limits = c(0, 100)) +
    theme_bw() +
    labs(x="Sample size", y = "Power Factor AB")
  
  invisible(list(p1 = p1,
                 p2 = p2,
                 p3 = p3,
                 power_df = data.frame(paste("f = ",
                                             round(power_res$f_A,2),
                                             " ",
                                             round(power_res$f_B,2),
                                             " ", 
                                             round(power_res$f_AB,2),
                                             "\n",
                                             "r = ",
                                             rho_A,
                                             " ",
                                             rho_B,
                                             " ",
                                             rho_AB), 
                                       n_vec, 
                                       power_A, 
                                       power_B, 
                                       power_AB)))
}