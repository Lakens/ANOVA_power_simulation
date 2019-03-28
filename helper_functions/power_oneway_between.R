power_oneway_between <- function(design_result, alpha_level=0.05){
  mean_mat <- t(matrix(design_result$mu, 
                       nrow = length(design_result$mu)/design_result$factors,
                       ncol = design_result$factors)) #Create a mean matrix
  colnames(mean_mat) <- design_result$design_list
  rownames(mean_mat) <- design_result$factornames
  
  # Using the sweep function to remove rowmeans from the matrix
  mean_mat_res <- sweep(mean_mat,2, rowMeans(mean_mat))   
  mean_mat_res
  MS_a <- design_result$n * (sum(mean_mat_res^2)/(length(design_result$mu)-1))
  SS_A <- design_result$n * sum(mean_mat_res^2)
  MS_error <- design_result$sd^2
  SS_error <- MS_error * (design_result$n*length(design_result$mu)) 
  df1 <- length(design_result$mu)-1
  df2 <- (design_result$n*length(design_result$mu) - length(design_result$mu))
  eta_p_2 <- SS_A/(SS_A+SS_error)
  f_2 <- eta_p_2/(1-eta_p_2)
  lambda <- f_2 * design_result$n * length(design_result$mu)
  # Cohen_f <- sqrt(sum(mean_mat_res^2)/length(design_result$mu))/sd #based on G*power manual page 28
  # We just take the sqrt(f_2) because formula above assumes maximum difference of means.
  Cohen_f <- sqrt(f_2)
  F_critical <- qf(alpha_level, df1, df2, lower.tail=FALSE) # Critical F-Value
  power <- pf(F_critical, df1, df2, lambda, lower.tail = FALSE) # power
  
  invisible(list(mu = design_result$n,
                 sigma = design_result$sd,
                 n = design_result$n, 
                 alpha_level = alpha_level,
                 Cohen_f = Cohen_f,
                 f_2 = f_2,
                 lambda = lambda,
                 F_critical = F_critical,
                 power = power,
                 df1 = df1,
                 df2 = df2,
                 eta_p_2 = eta_p_2,
                 mean_mat = mean_mat))
}