#ANOVA power function; last update: April 19 2019
#MANOVA output included
ANOVA_power <- function(design_result, alpha_level = .05, p_adjust = "none", nsims){
  
  #Require necessary packages
  require(mvtnorm, quietly = TRUE)
  require(afex, quietly = TRUE)
  require(emmeans, quietly = TRUE)
  require(ggplot2, quietly = TRUE)
  require(gridExtra, quietly = TRUE)
  require(reshape2, quietly = TRUE)
  
  options(scipen = 999) # 'turn off' scientific notation
  
  effect_size_d <- function(x, y, conf.level = 0.95){ 
    sd1 <- sd(x) #standard deviation of measurement 1
    sd2 <- sd(y) #standard deviation of measurement 2
    n1 <- length(x) #number of pairs
    n2 <- length(y) #number of pairs
    df <- n1 + n2 - 2
    m_diff <- mean(y - x)
    sd_pooled <- (sqrt((((n1 - 1) * ((sd1^2))) + (n2 - 1) * ((sd2^2))) / ((n1 + n2 - 2)))) #pooled standard deviation
    #Calculate Hedges' correction. Uses gamma, unless this yields a nan (huge n), then uses approximation
    j <- (1 - 3/(4 * (n1 + n2 - 2) - 1))
    t_value <- m_diff / sqrt(sd_pooled^2 / n1 + sd_pooled^2 / n2)
    p_value = 2*pt(-abs(t_value), 
                   df = df)
    
    d <- m_diff / sd_pooled #Cohen's d
    d_unb <- d*j #Hedges g, of unbiased d
    
    invisible(list(d = d, 
                   d_unb = d_unb, 
                   p_value = p_value))
  }
  
  effect_size_d_paired <- function(x, y, conf.level = 0.95){ 
    sd1 <- sd(x) #standard deviation of measurement 1
    sd2 <- sd(y) #standard deviation of measurement 2
    s_diff <- sd(x - y) #standard deviation of the difference scores
    N <- length(x) #number of pairs
    df = N - 1
    s_av <- sqrt((sd1 ^ 2 + sd2 ^ 2) / 2) #averaged standard deviation of both measurements
    
    #Cohen's d_av, using s_av as standardizer
    m_diff <- mean(y - x)
    d_av <- m_diff / s_av
    d_av_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_av
    
    #get the t-value for the CI
    t_value <- m_diff / (s_diff / sqrt(N))
    p_value = 2 * pt(-abs(t_value),
                     df = df)
    
    #Cohen's d_z, using s_diff as standardizer
    d_z <- t_value / sqrt(N)
    d_z_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_z
    
    invisible(list(
      d_z = d_z,
      d_z_unb = d_z_unb,
      p_value = p_value
    ))
  }
  
  #Check to ensure there is a within subject factor -- if none --> no MANOVA
  run_manova <- grepl("w", design_result$string)
  
  #Only utilized if MANOVA output included (see run_manova)
  Anova.mlm.table <- function(x, ...)
  {
    test <- x$test
    repeated <- x$repeated
    ntests <- length(x$terms)
    tests <- matrix(NA, ntests, 4)
    if (!repeated)
      SSPE.qr <- qr(x$SSPE)
    for (term in 1:ntests) {
      eigs <- Re(eigen(qr.coef(if (repeated) qr(x$SSPE[[term]])  
                               else SSPE.qr,
                               x$SSP[[term]]), symmetric = FALSE)$values)
      tests[term, 1:4] <- switch(test, Pillai = stats:::Pillai(eigs,
                                                               x$df[term], x$error.df), Wilks = stats:::Wilks(eigs,
                                                                                                              x$df[term], x$error.df), `Hotelling-Lawley` =  
                                   stats:::HL(eigs,
                                              x$df[term], x$error.df), Roy = stats:::Roy(eigs,
                                                                                         x$df[term], x$error.df))
    }
    ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
    ok <- !is.na(ok) & ok
    tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3],
                                   tests[ok, 4], lower.tail = FALSE))
    rownames(tests) <- x$terms
    colnames(tests) <- c("df", "test_stat", "approx_F", "num_Df",
                         "den_Df", "p.value")
    tests <- structure(as.data.frame(tests), heading = paste("\nType ",
                                                             x$type, if (repeated)
                                                               " Repeated Measures", " MANOVA Tests: ", test, " test  
                                                             statistic",
                                                             sep = ""), class = c("anova", "data.frame"))
    invisible(tests)
  }
  
  round_dig <- 4 #Set digits to which you want to round the output. 
  
  if (missing(alpha_level)) {
    alpha_level <- 0.05
  }
  string <- design_result$string #String used to specify the design
  
  factornames <- design_result$factornames #Get factor names
  
  # Specify the parameters you expect in your data (sd, r for within measures)
  
  #number of subjects you will collect (for each between factor) 
  # For an all within design, this is total N
  # For a 2b*2b design, this is the number of people in each between condition, so in each of 2*2 = 4 groups 
  
  n <- design_result$n
  
  # specify population means for each condition (so 2 values for 2b design, 6 for 2b*3w, etc) 
  mu = design_result$mu # population means - should match up with the design
  
  sd <- design_result$sd #population standard deviation (currently assumes equal variances)
  r <- design_result$r # correlation between within factors (currently only 1 value can be entered)
  
  #indicate which adjustment for multiple comparisons you want to use (e.g., "holm")
  p_adjust <- design_result$p_adjust
  
  
  
  ###############
  # 2. Create Dataframe based on Design ----
  ###############
  
  #Count number of factors in design
  factors <- design_result$factors
  
  #Specify within/between factors in design: Factors that are within are 1, between 0
  design <- design_result$design
  
  sigmatrix <- design_result$sigmatrix
  
  #Create the data frame. This will be re-used in the simulation (y variable is overwritten) but created only once to save time in the simulation
  df <- design_result$df
  
  ###############
  # 3. Specify factors for formula ----
  ###############
  
  frml1 <- design_result$frml1 
  frml2 <- design_result$frml2
  
  aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                                          data = df, include_aov = FALSE,
                                          anova_table = list(es = "pes", p_adjust_method = p_adjust)) }) #This reports PES not GES
  
  #Run MANOVA if within subject factor is included; otherwise ignored
  if (run_manova == TRUE) {
    manova_result <- Anova.mlm.table(aov_result$Anova)
  }
  
  
  ############################################
  #Specify factors for formula ###############
  design_list <- design_result$design_list
  
  ###############
  # 5. Set up dataframe for simulation results
  ###############
  
  
  
  #How many possible planned comparisons are there (to store p and es)
  possible_pc <- (((prod(
    as.numeric(strsplit(string, "\\D+")[[1]])
  )) ^ 2) - prod(as.numeric(strsplit(string, "\\D+")[[1]])))/2
  
  
  if (run_manova == TRUE) {
    #create empty dataframe to store simulation results
    #number of columns if for ANOVA results and planned comparisons, times 2 (p and es)
    #more columns added if MANOVA output included 2^factors
    sim_data <- as.data.frame(matrix(
      ncol = 2 * (2 ^ factors - 1) + (2 ^ factors) + 2 * possible_pc,
      nrow = nsims
    )) } else {
      
      sim_data <- as.data.frame(matrix(
        ncol = 2 * (2 ^ factors - 1) + 2 * possible_pc,
        nrow = nsims
      ))
      
    }
  
  #set up paired tests
  # Moved to DESIGN
  # #Need to identify which columns from df to pull the factor names from
  # if (design_result$factors == 1) {
  #   cond_col <- c(4)
  # } else if (design_result$factors == 2) {
  #   cond_col <- c(4, 5)
  # } else {
  #   cond_col <- c(4, 5, 6)
  # }
  # 
  # df$cond <- as.character(interaction(df[, cond_col], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)
  paired_tests <- combn(unique(df$cond),2)
  paired_p <- numeric(possible_pc)
  paired_d <- numeric(possible_pc)
  within_between <- sigmatrix[upper.tri(sigmatrix)] #based on whether correlation is 0 or not, we can determine if we should run a paired or independent t-test
  
  #Dynamically create names for the data we will store
  #Again create rownames based on whether or not a MANOVA should be included
  if (run_manova == TRUE) {
    names(sim_data) = c(paste("anova_",
                              rownames(aov_result$anova_table), 
                              sep = ""), 
                        paste("anova_es_", 
                              rownames(aov_result$anova_table), 
                              sep = ""), 
                        paste("p_", 
                              paste(paired_tests[1,],paired_tests[2,],sep = "_"), 
                              sep = ""), 
                        paste("d_", 
                              paste(paired_tests[1,],paired_tests[2,], sep = "_"), 
                              sep = ""),
                        paste("manova_",
                              rownames(manova_result), 
                              sep = ""))
  } else {
    names(sim_data) = c(paste("anova_",
                              rownames(aov_result$anova_table), 
                              sep = ""), 
                        paste("anova_es_", 
                              rownames(aov_result$anova_table), 
                              sep = ""), 
                        paste("p_", 
                              paste(paired_tests[1,],paired_tests[2,],sep = "_"), 
                              sep = ""), 
                        paste("d_", 
                              paste(paired_tests[1,],paired_tests[2,], sep = "_"), 
                              sep = ""))
  }
  
  
  ###############
  # 7. Start Simulation ----
  ###############
  #withProgress(message = 'Running simulations', value = 0, { #block outside of Shiny
  for (i in 1:nsims) { #for each simulated experiment
    #incProgress(1/nsims, detail = paste("Now running simulation", i, "out of",nsims,"simulations")) #Block outside of Shiny
    #We simulate a new y variable, melt it in long format, and add it to the df (surpressing messages)
    df$y <- suppressMessages({
      melt(as.data.frame(rmvnorm(
        n = n,
        mean = mu,
        sigma = as.matrix(sigmatrix)
      )))$value
    })
    
    # We perform the ANOVA using AFEX
    #Can be set to NICE to speed up, but required data grabbing from output the change. 
    aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                                            data = df, include_aov = FALSE, #Need development code to get aov_include function
                                            anova_table = list(es = "pes", 
                                                               p_adjust_method = p_adjust))}) #This reports PES not GES
    
    # Store MANOVA result if there are within subject factors
    if (run_manova == TRUE) {
      manova_result <- Anova.mlm.table(aov_result$Anova) 
    }
    
    for (j in 1:possible_pc) {
      x <- df$y[which(df$cond == paired_tests[1,j])]
      y <- df$y[which(df$cond == paired_tests[2,j])]
      #this can be sped up by tweaking the functions that are loaded to only give p and dz
      ifelse(within_between[j] == 0,
             t_test_res <- effect_size_d(x, y, conf.level = 1 - alpha_level),
             t_test_res <- effect_size_d_paired(x, y, conf.level = 1 - alpha_level))
      paired_p[j] <- t_test_res$p_value
      paired_d[j] <- ifelse(within_between[j] == 0,
                            t_test_res$d,
                            t_test_res$d_z)
    }
    
    # store p-values and effect sizes for calculations and plots.
    #If needed to create different row names if MANOVA is included
    if (run_manova == TRUE) {
      sim_data[i,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                        aov_result$anova_table[[5]], #partial eta squared
                        paired_p, #p-values for paired comparisons
                        paired_d, #effect sizes
                        manova_result[[6]]) #p-values for MANOVA
    } else {
      sim_data[i,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                        aov_result$anova_table[[5]], #partial eta squared
                        paired_p, #p-values for paired comparisons
                        paired_d) #effect sizes
    }
  }
  #}) #close withProgress Block outside of Shiny
  
  ############################################
  #End Simulation              ###############
  
  
  ###############
  # 8. Plot Results ----
  ###############
  
  # melt the data into a long format for plots in ggplot2
  
  plotData <-
    suppressMessages({
      melt(sim_data[1:(2 ^ factors - 1)], value.name = 'p')
    })
  
  SalientLineColor <- "#535353"
  LineColor <- "#D0D0D0"
  BackgroundColor <- "#F0F0F0"
  
  # plot each of the p-value distributions 
  
  plt1 = ggplot(plotData, aes(x = p)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1),
                       labels = seq(0, 1, by = .1)) +
    geom_histogram(colour = "#535353",
                   fill = "#84D5F0",
                   breaks = seq(0, 1, by = .01)) +
    geom_vline(xintercept = alpha_level, colour = 'red') +
    facet_grid(variable ~ .) +
    labs(x = expression(p)) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    theme(panel.background = element_rect(fill = BackgroundColor)) +
    theme(plot.background = element_rect(fill = BackgroundColor)) +
    theme(panel.border = element_rect(colour = BackgroundColor)) +
    theme(panel.grid.major = element_line(colour = LineColor, size = .75)) +
    theme(plot.title = element_text(
      face = "bold",
      colour = SalientLineColor,
      vjust = 2,
      size = 20
    )) +
    theme(axis.text.x = element_text(
      size = 10,
      colour = SalientLineColor,
      face = "bold"
    )) +
    theme(axis.text.y = element_text(
      size = 10,
      colour = SalientLineColor,
      face = "bold"
    )) +
    theme(axis.title.y = element_text(
      size = 12,
      colour = SalientLineColor,
      face = "bold",
      vjust = 2
    )) +
    theme(axis.title.x = element_text(
      size = 12,
      colour = SalientLineColor,
      face = "bold",
      vjust = 0
    )) +
    theme(axis.ticks.x = element_line(colour = SalientLineColor, size =
                                        2)) +
    theme(axis.ticks.y = element_line(colour = BackgroundColor)) +
    theme(axis.line = element_line()) +
    theme(axis.line.x = element_line(size = 1.2, colour = SalientLineColor)) +
    theme(axis.line.y = element_line(colour = BackgroundColor)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  
  #Plot p-value distributions for simple comparisons
  # melt the data into a ggplot friendly 'long' format
  p_paired <- sim_data[(2 * (2 ^ factors - 1) + 1):(2 * (2 ^ factors - 1) + possible_pc)]
  
  plotData <- suppressMessages(melt(p_paired, value.name = 'p'))
  
  # plot each of the p-value distributions
  plt2 = ggplot(plotData, aes(x = p)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1),
                       labels = seq(0, 1, by = .1)) +
    geom_histogram(colour = "#535353",
                   fill = "#84D5F0",
                   breaks = seq(0, 1, by = .01)) +
    geom_vline(xintercept = alpha_level, colour = 'red') +
    facet_grid(variable ~ .) +
    labs(x = expression(p)) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    theme(panel.background = element_rect(fill = BackgroundColor)) +
    theme(plot.background = element_rect(fill = BackgroundColor)) +
    theme(panel.border = element_rect(colour = BackgroundColor)) +
    theme(panel.grid.major = element_line(colour = LineColor, size = .75)) +
    theme(plot.title = element_text(
      face = "bold",
      colour = SalientLineColor,
      vjust = 2,
      size = 20
    )) +
    theme(axis.text.x = element_text(
      size = 10,
      colour = SalientLineColor,
      face = "bold"
    )) +
    theme(axis.text.y = element_text(
      size = 10,
      colour = SalientLineColor,
      face = "bold"
    )) +
    theme(axis.title.y = element_text(
      size = 12,
      colour = SalientLineColor,
      face = "bold",
      vjust = 2
    )) +
    theme(axis.title.x = element_text(
      size = 12,
      colour = SalientLineColor,
      face = "bold",
      vjust = 0
    )) +
    theme(axis.ticks.x = element_line(colour = SalientLineColor, size =
                                        2)) +
    theme(axis.ticks.y = element_line(colour = BackgroundColor)) +
    theme(axis.line = element_line()) +
    theme(axis.line.x = element_line(size = 1.2, colour = SalientLineColor)) +
    theme(axis.line.y = element_line(colour = BackgroundColor)) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  
  ###############
  # 9. Sumary of power and effect sizes of main effects and contrasts ----
  ###############
  
  #Main effects and interactions from the ANOVA
  power = as.data.frame(apply(as.matrix(sim_data[(1:(2 ^ factors - 1))]), 2, 
                              function(x) round(mean(ifelse(x < alpha_level, 1, 0) * 100),round_dig)))
  
  es = as.data.frame(apply(as.matrix(sim_data[((2^factors):(2 * (2 ^ factors - 1)))]), 2, 
                           function(x) round(median(x),round_dig)))
  
  main_results <- data.frame(power,es)
  names(main_results) = c("power","effect size")
  
  
  
  #Data summary for pairwise comparisons
  power_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + 1):(2 * (2 ^ factors - 1) + possible_pc)]), 2,  
                                     function(x) round(mean(ifelse(x < alpha_level, 1, 0) * 100),round_dig)))
  
  es_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + possible_pc + 1):(2*(2 ^ factors - 1) + 2 * possible_pc)]), 2, 
                                  function(x) round(mean(x),round_dig)))
  
  pc_results <- data.frame(power_paired, es_paired)
  names(pc_results) = c("power","effect size")
  
  
  #Simulation results from MANOVA
  if (run_manova == TRUE) {
    power_MANOVA = as.data.frame(apply(as.matrix(sim_data[((2*(2 ^ factors - 1) + 2 * possible_pc + 1):(2 ^ factors + (2*(2 ^ factors - 1) + 2 * possible_pc)))]), 2, 
                                       function(x) round(mean(ifelse(x < alpha_level, 1, 0) * 100),round_dig)))
    
    manova_result <- data.frame(power_MANOVA)
    names(manova_result) = c("power")
  }
  
  #######################
  # Return Results ----
  #######################
  
  # The section below should be blocked out when in Shiny
  cat("Power and Effect sizes for ANOVA tests")
  cat("\n")
  print(main_results)
  cat("\n")
  cat("Power and Effect sizes for contrasts")
  cat("\n")
  print(pc_results)
  if (run_manova == TRUE) {
    cat("\n")
    cat("Within-Subject Factors Included: Check MANOVA Results")
  }
  
  #Create empty value if no MANOVA results are included
  if (run_manova == FALSE) {
    manova_result = NULL
  }
  
  # Return results in list()
  invisible(list(sim_data = sim_data,
                 main_results = main_results,
                 pc_results = pc_results,
                 manova_result = manova_result,
                 plot1 = plt1,
                 plot2 = plt2,
                 p_adjust = p_adjust,
                 nsims = nsims,
                 alpha_level = alpha_level))
  
}
