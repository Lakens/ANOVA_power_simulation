calc_error_rate_main <- function(power_result, alpha_level=0.05){
  sum(apply(as.matrix(power_result$sim_data[(1:length(power_result$main_results$power))]), 1, 
            function(x) round(mean(ifelse(x < alpha_level, 1, 0)),4)) > 0)/power_result$nsims*100
}

calc_error_rate_pc <- function(power_result, alpha_level=0.05){
  sum(apply(as.matrix(power_result$sim_data[(2*length(power_result$main_results$power)+1:length(power_result$pc_results$power))]), 1, 
            function(x) round(mean(ifelse(x < alpha_level, 1, 0)),4)) > 0)/power_result$nsims*100
}


xxxx <- as.matrix(power_result$sim_data[(2*length(power_result$main_results$power)+1:length(power_result$pc_results$power))])
