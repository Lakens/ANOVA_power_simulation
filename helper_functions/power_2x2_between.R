power_2x2_between <- function(design_result, alpha_level=0.05){
  mean_mat <- t(matrix(design_result$mu, 
                       nrow = length(design_result$mu)/design_result$factors,
                       ncol = design_result$factors)) #Create a mean matrix
  colnames(mean_mat) <- design_result$labelnames[[1]]
  rownames(mean_mat) <- design_result$labelnames[[2]]
  
  mean_mat_AB <- mean_mat - (mean(mean_mat) + sweep(mean_mat,1, rowMeans(mean_mat)) + sweep(mean_mat,2, colMeans(mean_mat)))
  
  MS_A <- design_result$n * length(design_result$labelnames[[1]]) * (sum((colMeans(mean_mat) - mean(mean_mat))^2)/(length(design_result$labelnames[[1]])-1))
  SS_A <- design_result$n * length(design_result$labelnames[[1]]) * sum((colMeans(mean_mat) - mean(mean_mat))^2)

  MS_B <- design_result$n * length(design_result$labelnames[[2]]) * (sum((rowMeans(mean_mat) - mean(mean_mat))^2)/(length(design_result$labelnames[[2]])-1))
  SS_B <- design_result$n * length(design_result$labelnames[[2]]) * sum((rowMeans(mean_mat) - mean(mean_mat))^2)
  
  MS_AB <- design_result$n * sum(mean_mat_AB^2)/((length(design_result$labelnames[[1]])-1) * (length(design_result$labelnames[[2]])-1))
  SS_AB <- design_result$n * sum(mean_mat_AB^2)

  MS_error <- design_result$sd^2
  SS_error <- MS_error * (design_result$n*length(design_result$mu)) 

  # For main effect A
  df1_A <- (length(design_result$labelnames[[1]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  df2 <- (design_result$n*length(design_result$mu) - length(design_result$mu))
  eta_p_2_A <- SS_A/(SS_A+SS_error)
  f_2_A <- eta_p_2_A/(1-eta_p_2_A)
  # f_A <- sqrt(sum((rowMeans(mean_mat) - mean(mean_mat))^2)/length(design_result$labelnames[[1]]))/design_result$sd 
  # Based on Cohen, 8.2.2, p275. But this works only when means are miximally different. 
  # Therefore we just take the square root for f_2_A
  Cohen_f_A <- sqrt(f_2_A)
  lambda_A <- design_result$n * length(design_result$labelnames[[1]]) * sum((rowMeans(mean_mat)-mean(rowMeans(mean_mat)))^2)/design_result$sd^2
  F_critical_A <- qf(alpha_level, # critical F-vaue
                     df1_A,
                     df2, 
                     lower.tail=FALSE) 
  power_A <- pf(qf(alpha_level, #powerer 
                   df1_A, 
                   df2, 
                   lower.tail = FALSE), 
                df1_A, 
                df2, 
                lambda_A, 
                lower.tail = FALSE)
  
  # For main effect B
  df1_B <- (length(design_result$labelnames[[2]]) - 1) #calculate degrees of freedom 1 - ignoring the * e sphericity correction
  eta_p_2_A <- SS_B/(SS_B+SS_error)
  f_2_B <- eta_p_2_B/(1-eta_p_2_B)
  Cohen_f_B <- sqrt(f_2_B)
  lambda_B <- design_result$n * length(design_result$labelnames[[2]]) * sum((colMeans(mean_mat)-mean(colMeans(mean_mat)))^2)/design_result$sd^2
  F_critical_B <- qf(alpha_level, # critical F-vaue
                     df1_B,
                     df2, 
                     lower.tail=FALSE) 
  power_B <- pf(qf(alpha_level, #powerer 
                   df1_B, 
                   df2, 
                   lower.tail = FALSE), 
                df1_B, 
                df2, 
                lambda_B, 
                lower.tail = FALSE)
  
  # For main effect AB
  df1_AB <- (length(design_result$labelnames[[1]])-1) * (length(design_result$labelnames[[2]])-1)
  eta_p_2_AB <- SS_AB/(SS_AB+SS_error)
  Cohen_f_2_AB <- eta_p_2_AB/(1-eta_p_2_AB)
  lambda_AB <- design_result$n * length(design_result$labelnames[[1]]) * length(design_result$labelnames[[2]]) * Cohen_f_2_AB
  F_critical_AB <- qf(alpha_level, 
                      df1_AB, 
                      df2, 
                      lower.tail=FALSE) # Critical F-Value
  power_AB <- pf(qf(alpha_level, #powerer 
                   df1_AB, 
                   df2, 
                   lower.tail = FALSE), 
                df1_AB, 
                df2, 
                lambda_AB, 
                lower.tail = FALSE)

  invisible(list(mu = design_result$n,
                 sigma = design_result$sd,
                 n = design_result$n, 
                 alpha_level = alpha_level,
                 Cohen_f_A = Cohen_f_A,
                 Cohen_f_B = Cohen_f_B,
                 Cohen_f_AB = Cohen_f_AB,
                 f_2_A = f_2_A,
                 f_2_B = f_2_B,
                 f_2_AB = f_2_AB,
                 lambda_A = lambda_A,
                 lambda_B = lambda_B,
                 lambda_AB = lambda_AB,
                 F_critical_A = F_critical_A,
                 F_critical_B = F_critical_B,
                 F_critical_AB = F_critical_AB,
                 power_A = power_A,
                 power_B = power_B,
                 power_AB = power_AB,
                 df1_A = df1_A,
                 df1_B = df1_B,
                 df1_AB = df1_AB,
                 df2 = df2,
                 eta_p_2_A = eta_p_2_A,
                 eta_p_2_B = eta_p_2_B,
                 eta_p_2_AB = eta_p_2_AB,
                 mean_mat = mean_mat))
}