ANOVA_power <- function(ANOVA_design, nsims){
  string <- ANOVA_design$string #String used to specify the design
  
  # Specify the parameters you expect in your data (sd, r for within measures)
  
  #number of subjects you will collect (for each between factor) 
  # For an all within design, this is total N
  # For a 2b*2b design, this is the number of people in each between condition, so in each of 2*2 = 4 groups 
  
  n<-ANOVA_design$n
  
  # specify population means for each condition (so 2 values for 2b design, 6 for 2b*3w, etc) 
  mu = ANOVA_design$mu # population means - should match up with the design
  
  sd <- ANOVA_design$sd #population standard deviation (currently assumes equal variances)
  r <- ANOVA_design$r # correlation between within factors (currently only 1 value can be entered)
  
  #indicate which adjustment for multiple comparisons you want to use (e.g., "holm")
  p_adjust <- ANOVA_design$p_adjust
  
  # how many studies should be simulated? 100.000 is very accurate, 10.000 reasonable accurate, 10.000 somewhat accurate
  nsims = 100
  
  ###############
  # 2. Load libraries ----
  ###############
  
  library(mvtnorm)
  library(afex)
  library(lsmeans)
  library(ggplot2)
  library(psych)
  library(tidyr)
  require(gridExtra)
  require(reshape2)
  require(sjstats)
  
  ###############
  # 2. Create Dataframe based on Design ----
  ###############
  
  #Count number of factors in design
  factors <- ANOVA_design$factors
  
  #Specify within/between factors in design: Factors that are within are 1, between 0
  design <- ANOVA_design$design
  
  sigmatrix <- ANOVA_design$sig
  
  #Create the data frame. This will be re-used in the simulation (y variable is overwritten) but created only once to save time in the simulation
  df <- ANOVA_design$df
  
  ###############
  # 3. Specify factors for formula ----
  ###############
  
  frml1 <- ANOVA_design$frml1 
  frml2 <- ANOVA_design$frml2
  
  aov_result<-aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                      data=df,
                      anova_table = list(es = "pes", p_adjust_method = p_adjust)) #This reports PES not GES
  
  # pairwise comparisons
  pc <- pairs(emmeans(aov_result, frml2), adjust = p_adjust)
  
  ############################################
  #Specify factors for formula ###############
  design_list <- ANOVA_design$design_list
  
  ###############
  # 5. Set up dataframe for simulation results
  ###############
  
  #How many possible planned comparisons are there (to store p and es)
  possible_pc <- (((prod(as.numeric(strsplit(string, "\\D+")[[1]])))^2)-prod(as.numeric(strsplit(string, "\\D+")[[1]])))/2
  
  #create empty dataframe to store simulation results
  #number of columns if for ANOVA results and planned comparisons, times 2 (p and es)
  sim_data <- as.data.frame(matrix(ncol = 2*(2^factors-1)+2*possible_pc, nrow = nsims))
  
  #Dynamically create names for the data we will store
  names(sim_data) = c(paste("anova_p_",
                            rownames(aov_result$anova_table), 
                            sep=""), 
                      paste("anova_es_", 
                            rownames(aov_result$anova_table), 
                            sep=""), 
                      paste("paired_comparison_p_", 
                            pc@grid[["contrast"]], 
                            sep=""), 
                      paste("d_", 
                            pc@grid[["contrast"]], 
                            sep=""))
  
  
  ###############
  # 7. Start Simulation ----
  ###############
  
  pb <- winProgressBar(title = "progress bar", min = 0, max = nsims, width = 300)
  i=1
  for(i in 1:nsims){ #for each simulated experiment
    setWinProgressBar(pb, i, title=paste( round(i/nsims*100, 0),
                                          "% done"))
    #We simulate a new y variable, melt it in long format, and add it to the df (surpressing messages)
    df$y<-suppressMessages({melt(as.data.frame(rmvnorm(n=n,
                               mean=mu,
                               sigma=sigmatrix)))$value
    })
  
    # We perform the ANOVA using AFEX
    aov_result<-suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                        data=df,
                        anova_table = list(es = "pes", p_adjust_method = p_adjust))}) #This reports PES not GES
    # pairwise comparisons
    pc <- pairs(emmeans(aov_result, frml2), adjust = p_adjust)
    # store p-values and effect sizes for calculations and plots.
    sim_data[i,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                    aov_result$anova_table[[5]], #partial eta squared
                    as.data.frame(summary(pc))$p.value, #p-values for paired comparisons
                    as.data.frame(summary(pc))$t.ratio/sqrt(n)) #Cohen's dz
  }
  
  close(pb) #close the progress bar
  
  ############################################
  #End Simulation              ###############
  
  
  ###############
  # 8. Plot Results ----
  ###############
  
  # melt the data into a long format for plots in ggplot2
  
  plotData <- melt(sim_data[1:(2^factors-1)], value.name = 'p')
  
  SalientLineColor<-"#535353"
  LineColor<-"#D0D0D0"
  BackgroundColor<-"#F0F0F0"
  
  # plot each of the p-value distributions 
  options(scipen = 999) # 'turn off' scientific notation
  plt1 = ggplot(plotData, aes(x = p)) +
    scale_x_continuous(breaks=seq(0, 1, by = .1),
                       labels=seq(0, 1, by = .1)) +
    geom_histogram(colour="#535353", fill="#84D5F0", breaks=seq(0, 1, by = .01)) +
    geom_vline(xintercept = 0.05, colour='red') +
    facet_grid(variable ~ .) +
    labs(x = expression(p)) +
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank()) + 
    theme(panel.background=element_rect(fill=BackgroundColor)) +
    theme(plot.background=element_rect(fill=BackgroundColor)) +
    theme(panel.border=element_rect(colour=BackgroundColor)) + 
    theme(panel.grid.major=element_line(colour=LineColor,size=.75)) + 
    theme(plot.title=element_text(face="bold",colour=SalientLineColor, vjust=2, size=20)) + 
    theme(axis.text.x=element_text(size=10,colour=SalientLineColor, face="bold")) +
    theme(axis.text.y=element_text(size=10,colour=SalientLineColor, face="bold")) +
    theme(axis.title.y=element_text(size=12,colour=SalientLineColor,face="bold", vjust=2)) +
    theme(axis.title.x=element_text(size=12,colour=SalientLineColor,face="bold", vjust=0)) + 
    theme(axis.ticks.x=element_line(colour=SalientLineColor, size=2)) +
    theme(axis.ticks.y=element_line(colour=BackgroundColor)) +
    theme(axis.line = element_line()) +
    theme(axis.line.x=element_line(size=1.2,colour=SalientLineColor)) +
    theme(axis.line.y=element_line(colour=BackgroundColor)) + 
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
  plt1
  
  
  # #Plot p-value distributions for simple comparisons
  # # melt the data into a ggplot friendly 'long' format
  # p_paired <- sim_data[(2*(2^factors-1)+1):(2*(2^factors-1)+possible_pc)]
  # 
  # plotData <- melt(p_paired, value.name = 'p')
  # 
  # # plot each of the p-value distributions 
  # plt2 = ggplot(plotData, aes(x = p)) +
  #   scale_x_continuous(breaks=seq(0, 1, by = .1),
  #                      labels=seq(0, 1, by = .1)) +
  #   geom_histogram(colour="#535353", fill="#84D5F0", breaks=seq(0, 1, by = .01)) +
  #   geom_vline(xintercept = 0.05, colour='red') +
  #   facet_grid(variable ~ .) +
  #   labs(x = expression(p)) +
  #   theme_bw() + 
  #   theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank()) + 
  #   theme(panel.background=element_rect(fill=BackgroundColor)) +
  #   theme(plot.background=element_rect(fill=BackgroundColor)) +
  #   theme(panel.border=element_rect(colour=BackgroundColor)) + 
  #   theme(panel.grid.major=element_line(colour=LineColor,size=.75)) + 
  #   theme(plot.title=element_text(face="bold",colour=SalientLineColor, vjust=2, size=20)) + 
  #   theme(axis.text.x=element_text(size=10,colour=SalientLineColor, face="bold")) +
  #   theme(axis.text.y=element_text(size=10,colour=SalientLineColor, face="bold")) +
  #   theme(axis.title.y=element_text(size=12,colour=SalientLineColor,face="bold", vjust=2)) +
  #   theme(axis.title.x=element_text(size=12,colour=SalientLineColor,face="bold", vjust=0)) + 
  #   theme(axis.ticks.x=element_line(colour=SalientLineColor, size=2)) +
  #   theme(axis.ticks.y=element_line(colour=BackgroundColor)) +
  #   theme(axis.line = element_line()) +
  #   theme(axis.line.x=element_line(size=1.2,colour=SalientLineColor)) +
  #   theme(axis.line.y=element_line(colour=BackgroundColor)) + 
  #   theme(plot.margin = unit(c(1,1,1,1), "cm"))
  # plt2
  
  ###############
  # 9. Sumary of power and effect sizes of main effects and contrasts ----
  ###############
  
  #Main effects and interactions from the ANOVA
  power = as.data.frame(apply(as.matrix(sim_data[(1:(2^factors-1))]), 2, 
                                     function(x) round(mean(ifelse(x < .05, 1, 0) * 100),3)))
  es = as.data.frame(apply(as.matrix(sim_data[((2^factors):(2*(2^factors-1)))]), 2, 
                                  function(x) round(mean(x),3)))
  
  main_results <- data.frame(power,es)
  names(main_results) = c("power","effect size")
  main_results
  
  #Data summary for contrasts
  power_paired = as.data.frame(apply(as.matrix(sim_data[(2*(2^factors-1)+1):(2*(2^factors-1)+possible_pc)]), 2, 
                function(x) round(mean(ifelse(x < .05, 1, 0) * 100),2)))
  es_paired = as.data.frame(apply(as.matrix(sim_data[(2*(2^factors-1)+possible_pc+1):(2*(2^factors-1)+2*possible_pc)]), 2, 
                                     function(x) round(mean(x),2)))

  
  pc_results <- data.frame(power_paired,es_paired)
  names(pc_results) = c("power","effect size")
  pc_results
  
  #######################
  # Return Results ----
  #######################
  
  cat("Power and Effect sizes for ANOVA tests")
  cat("\n")
  print(main_results)
  cat("\n")
  cat("Power and Effect sizes for contrasts")
  cat("\n")
  print(pc_results)
  
  # Return results in list()
  invisible(list(sim_data = sim_data,
                 main_results = main_results,
                 pc_results = pc_results,
                 plot1 = plt1))
                 
}