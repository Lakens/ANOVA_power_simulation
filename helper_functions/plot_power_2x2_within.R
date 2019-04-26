plot_power_2x2_within <- function(design_result, max_n){
  
  string = design_result$string
  mu = design_result$mu
  sd <- design_result$sd
  r <- design_result$r
  p_adjust = design_result$p_adjust
  labelnames = c("A", "a1", "a2", "B", "b1", "b2")

  n_vec <- seq(from = 5, to = max_n)
  
  power_A <- numeric(length(n_vec))
  power_B <- numeric(length(n_vec))
  power_AB <- numeric(length(n_vec))
  
  for (i in 1:length(n_vec)){
    design_result <- ANOVA_design(string = string,
                                  n = n_vec[i], 
                                  mu = mu, 
                                  sd = sd, 
                                  r = r, 
                                  p_adjust = p_adjust,
                                  labelnames = labelnames)
    
    power_res <- power_2x2_within(design_result)
    
    power_A[i] <- power_res$power_A*100
    power_B[i] <- power_res$power_B*100
    power_AB[i] <- power_res$power_AB*100
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
                                             round(power_res$Cohen_f_A,2),
                                             " ",
                                             round(power_res$Cohen_f_B,2),
                                             " ", 
                                             round(power_res$Cohen_f_AB,2),
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