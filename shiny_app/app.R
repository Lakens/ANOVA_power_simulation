#May 17 2019 update: add updated functions


###############
# Load libraries 
###############


library(shiny)
library(mvtnorm)
library(afex)
library(emmeans)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(reshape2)
library(rmarkdown)
library(knitr)
library(MASS)

#TINYTEX will need to be installed on the server
#install.packages("tinytex")
#tinytex::install_tinytex(force = TRUE) 
#library(tinytex)

#ANOVA design function; last update: May 17 2019
#Update output; covariance output updated
ANOVA_design <- function(string, n, mu, sd, r = 0, labelnames = NULL, plot = FALSE){
  
  #Check String for an acceptable digits and factor (w or b)
  if (grepl("^(\\d{1,2}(w|b)\\*){0,2}\\d{1,2}(w|b)$", string, ignore.case = FALSE, perl = TRUE) == FALSE) {
    stop("Problem in the string argument: must input number of levels as integer (2-99) and factor-type (between or within) as lower case b (between) or w (within)")
  }
  
  #Ensure sd is greater than 0
  if (any(sd <= 0)) {
    stop("Standard deviation (sd) is less than or equal to zero; input a value greater than zero")
  }
  
  #Ensure, if single correlation is input, that it is between 0 and 1
  if (any(r < 0) | any(r >=1) ) {
    stop("Correlation must be greater than 0 and less than 1")
  }
  
  #Ensure proper n input
  if (length(n) != 1 ) {
    stop("Only balanced designs allowed: n can only be one value")
  }
  
  #If labelnames are not provided, they are generated.
  #Store factor levels (used many times in the script, calculate once)
  factor_levels <- as.numeric(strsplit(string, "\\D+")[[1]])
  
  if (is.null(labelnames)) {
    for(i1 in 1:length(factor_levels)){
      labelnames <- append(labelnames,paste(paste(letters[i1]), sep = ""))
      for(i2 in 1:factor_levels[i1]){
        labelnames <- append(labelnames,paste(paste(letters[i1]), paste(i2), sep = ""))
      }
    }
  }
  
  if (length(labelnames) != length(factor_levels) + sum(factor_levels)) {
    stop("Design (string) does not match the length of the labelnames")
  }
  
  ###############
  # 1. Specify Design and Simulation----
  ###############
  # String used to specify the design
  # Add numbers for each factor with 2 levels, e.g., 2 for a factor with 2 levels
  # Add a 'w' after the number for within factors, and a 'b' for between factors
  # Separate factors with a * (asterisk)
  # Thus "2b*3w) is a design with 2 between levels, and 3 within levels
  
  #Check if design and means match up - if not, throw an error and stop
  if(prod(factor_levels) != length(mu)){stop("the length of the vector with means does not match the study design")}
  
  #Check if the design and sd match (either 1 or length of design)
  #if(length(sd) != 1 && prod(factor_levels) != length(sd)){stop("The SD must be a length of 1 or match the length of the study design")}
  
  #Check if the factors are of an acceptable number of levels
  if(any(factor_levels <= 0) == TRUE | any(factor_levels > 99) ) {
    stop("Each factor can only have between 2 and 99 levels")
  }
  
  ###############
  # 2. Create Factors and Design ----
  ###############
  
  #Count number of factors in design
  factors <- length(factor_levels)
  
  #Get factor names and labelnameslist
  labelnames1 <- labelnames[(1 + 1):(1+factor_levels[1])]
  if(factors > 1){labelnames2 <- labelnames[(factor_levels[1] + 3):((factor_levels[1] + 3) + factor_levels[2] - 1)]}
  if(factors > 2){labelnames3 <- labelnames[(factor_levels[2] + factor_levels[1] + 4):((factor_levels[2] + factor_levels[1] + 4) + factor_levels[3] - 1)]}
  
  factornames1 <- labelnames[1]
  if(factors > 1){factornames2 <- labelnames[factor_levels[1] + 2]}
  if(factors > 2){factornames3 <- labelnames[factor_levels[2] + factor_levels[1] + 3]}
  
  if(factors == 1){labelnameslist <- list(labelnames1)}
  if(factors == 2){labelnameslist <- list(labelnames1,labelnames2)}
  if(factors == 3){labelnameslist <- list(labelnames1,labelnames2,labelnames3)}
  
  if(factors == 1){factornames <- c(factornames1)}
  if(factors == 2){factornames <- c(factornames1,factornames2)}
  if(factors == 3){factornames <- c(factornames1,factornames2,factornames3)}
  
  #Specify within/between factors in design: Factors that are within are 1, between 0
  design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed = TRUE)[[1]]
  design <- as.numeric(design == "w") #if within design, set value to 1, otherwise to 0
  
  #Specify design list (similar as below)
  xxx <- data.frame(matrix(NA, nrow = prod(factor_levels), ncol = 0))
  for(j in 1:factors){
    xxx <- cbind(xxx, as.factor(unlist(rep(as.list(paste(labelnameslist[[j]],
                                                         sep="_")),
                                           each = prod(factor_levels)/prod(factor_levels[1:j]),
                                           times = prod(factor_levels)/prod(factor_levels[j:factors])
    ))))
  }
  design_list <- as.character(interaction(xxx[, 1:factors], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)
  
  ###############
  # 3. Create Correlation and Covariance Matrix ----
  ###############
  
  #Create empty matrix
  sigmatrix <- data.frame(matrix(ncol=length(mu), nrow = length(mu)))
  
  #NEW CODE, JUST FOR SINGLE correlation entry
  
  #single number
  cors <- r
  vars <- length(design_list)
  
  #Code by Lisa De Bruine. Allows multiple inputs for r - only use single value now.
  #from: https://github.com/debruine/faux/blob/master/R/rnorm_multi.R
  generate_cor_matrix  <- function(vars = 3, cors = 0, mu = 0, sd = 1) {
    if (length(mu) == 1) {
      mu <- rep(mu, vars)
    } else if (length(mu) != vars) {
      stop("the length of mu must be 1 or vars");
    }
    
    if (length(sd) == 1) {
      sd <- rep(sd, vars)
    } else if (length(sd) != vars) {
      stop("the length of sd must be 1 or vars");
    }
    
    # correlation matrix
    if (class(cors) == "numeric" & length(cors) == 1) {
      if (cors >=-1 & cors <=1) {
        cors = rep(cors, vars*(vars-1)/2)
      } else {
        stop("cors must be between -1 and 1")
      }
    }
    
    if (class(cors) == "matrix") {
      if (!is.numeric(cors)) {
        stop("cors matrix not numeric")
      } else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
        stop("cors matrix wrong dimensions")
      } else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
        stop("cors matrix not symmetric")
      } else {
        cor_mat <- cors
      }
    } else if (length(cors) == vars*vars) {
      cor_mat <- matrix(cors, vars)
    } else if (length(cors) == vars*(vars-1)/2) {
      # generate full matrix from vector of upper right triangle
      
      cor_mat <- matrix(nrow=vars, ncol = vars)
      upcounter = 1
      lowcounter = 1
      for (col in 1:vars) {
        for (row in 1:vars) {
          if (row == col) {
            # diagonal
            cor_mat[row, col] = 1
          } else if (row > col) {
            # lower left triangle
            cor_mat[row, col] = cors[lowcounter]
            lowcounter <- lowcounter + 1
          }
        }
      }
      for (row in 1:vars) {
        for (col in 1:vars) {
          if (row < col) {
            # upper right triangle
            cor_mat[row, col] = cors[upcounter]
            upcounter <- upcounter + 1
          }
        }
      }
    }
    
    # check matrix is positive definite
    tol <- 1e-08
    ev <- eigen(cor_mat, only.values = TRUE)$values
    if (sum(ev < tol)) {
      stop("correlation matrix not positive definite")
    }
    
    return(cor_mat)
  }
  cor_mat <- generate_cor_matrix(vars = vars, cors = cors)
  
  sd_for_sigma <- sd #added to prevent changing sd which is now passed on
  if (length(sd_for_sigma) == 1) {
    sd_for_sigma <- rep(sd_for_sigma, vars)
  } else if (length(sd_for_sigma) != vars) {
    stop("the length of sd_for_sigma must be 1 or vars");
  }
  
  sigma <- (sd_for_sigma %*% t(sd_for_sigma)) * cor_mat #Our earlier code had a bug, with SD on the diagonal. Not correct! Thanks Lisa.
  
  #General approach: For each factor in the list of the design, save the first item (e.g., a1b1)
  #Then for each factor in the design, if 1, set number to wildcard
  i1<-1
  i2<-1
  for(i1 in 1:length(design_list)){
    design_list_split <- unlist(strsplit(design_list[i1],"_"))
    #current_factor <- design_list_split[c(2,4,6)[1:length(design)]] #this creates a string of 2, 2,4 or 2,4,6 depending on the length of the design for below
    for(i2 in 1:length(design)){
      #We set each number that is within to a wildcard, so that all within participant factors are matched
      if(design[i2]==1){design_list_split[i2] <- "\\w+"}
    }
    sigmatrix[i1,]<-as.numeric(grepl(paste0(design_list_split, collapse="_"), design_list)) # compare factors that match with current factor, given wildcard, save list to sigmatrix
  }
  
  #Now multiply the matrix we just created (that says what is within, and what is between,  with the original covariance matrix)
  #So factors manipulated within are correlated, those manipulated between are not.
  cor_mat <- sigmatrix*cor_mat
  sigmatrix <- sigma*sigmatrix
  row.names(sigmatrix) <- design_list
  colnames(sigmatrix) <- design_list
  row.names(cor_mat) <- design_list
  colnames(cor_mat) <- design_list
  
  ###############
  # 4. Create Dataframe based on Design ----
  ###############
  
  #Create the data frame. This will be re-used in the simulation (y variable is overwritten) but created only once to save time in the simulation
  dataframe <- as.data.frame(mvrnorm(n=n,
                                     mu=mu,
                                     Sigma=sigmatrix,
                                     empirical = FALSE))
  dataframe$subject<-as.factor(c(1:n)) #create temp subject variable just for merging
  #Melt dataframe
  dataframe <- melt(dataframe,
                    id.vars = "subject",
                    variable.name = "cond",
                    value.name = "y")
  
  # Let's break this down - it's a bit tricky. First, we want to create a list of labelnames that will indicate the factors.
  # We are looping this over the number of factors.
  # This: factor_levels - takes the string used to specify the design and turn it in a list.
  # we take the labelnames and factornames and combine them
  # We repeat these each: n*(2^(factors-1)*2)/(2^j) and them times:  (2^j/2) to get a list for each factor
  # We then bind these together with the existing dataframe.
  for(j in 1:factors){
    dataframe <- cbind(dataframe, as.factor(unlist(rep(as.list(paste(factornames[[j]],
                                                                     labelnameslist[[j]],
                                                                     sep="_")),
                                                       each = n*prod(factor_levels)/prod(factor_levels[1:j]),
                                                       times = prod(factor_levels)/prod(factor_levels[j:factors])
    ))))
  }
  #Rename the factor variables that were just created
  names(dataframe)[4:(3+factors)] <- factornames[1:factors]
  
  #Create subject column
  subject <- 1:n #Set subject to 1 to the number of subjects collected
  
  for(j2 in length(design):1){ #for each factor in the design, from last to first
    #if w: repeat current string as often as the levels in the current factor (e.g., 3)
    #id b: repeat current string + max of current subject
    if(design[j2] == 1){subject <- rep(subject,factor_levels[j2])}
    subject_length <- length(subject) #store current length - to append to string of this length below
    if(design[j2] == 0){
      for(j3 in 2:factor_levels[j2]){
        subject <- append(subject,subject[1:subject_length]+max(subject))
      }
    }
  }
  
  #Overwrite subject columns in dataframe
  dataframe$subject <- subject
  #For the correlation matrix, we want the names of each possible comparison of means
  #Need to identify which columns from dataframe to pull the factor names from
  if (factors == 1) {
    cond_col <- c(4)
  } else if (factors == 2) {
    cond_col <- c(4, 5)
  } else {
    cond_col <- c(4, 5, 6)
  }
  
  dataframe$cond <- as.character(interaction(dataframe[, cond_col], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)
  
  ###############
  # 5. Specify factors for formula ----
  ###############
  if(factors == 1 & sum(design) == 1){frml1 <- as.formula(paste("y ~ ",factornames[1]," + Error(subject/",factornames[1],")",sep=""))}
  if(factors == 1 & sum(design) == 0){frml1 <- as.formula(paste("y ~ ",factornames[1]," + Error(1 | subject)",sep=""))}
  
  if(factors == 2){
    if(sum(design) == 2){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2]," + Error(subject/",factornames[1],"*",factornames[2],")"))}
    if(sum(design) == 0){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"  + Error(1 | subject)"))}
    if(all(design == c(1, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2]," + Error(subject/",factornames[1],")"))}
    if(all(design == c(0, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2]," + Error(subject/",factornames[2],")"))}
  }
  
  if(factors == 3){
    if(sum(design) == 3){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],"*",factornames[2],"*",factornames[3],")"))}
    if(sum(design) == 0){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(1 | subject)"))}
    if(all(design == c(1, 0, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],")"))}
    if(all(design == c(0, 1, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[2],")"))}
    if(all(design == c(0, 0, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[3],")"))}
    if(all(design == c(1, 1, 0)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],"*",factornames[2],")"))}
    if(all(design == c(0, 1, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[2],"*",factornames[3],")"))}
    if(all(design == c(1, 0, 1)) == TRUE){frml1 <- as.formula(paste("y ~ ",factornames[1],"*",factornames[2],"*",factornames[3]," + Error(subject/",factornames[1],"*",factornames[3],")"))}
  }
  
  #Specify second formula used for plotting
  if(factors == 1){frml2 <- as.formula(paste("~",factornames[1]))}
  if(factors == 2){frml2 <- as.formula(paste("~",factornames[1],"+",factornames[2]))}
  if(factors == 3){frml2 <- as.formula(paste("~",factornames[1],"+",factornames[2],"+",factornames[3]))}
  
  ###############
  # 6. Create plot of means to visualize the design ----
  ###############
  
  dataframe_means <- data.frame(mu, sd)
  for(j in 1:factors){
    dataframe_means <- cbind(dataframe_means, as.factor(unlist(rep(as.list(paste(labelnameslist[[j]],
                                                                                 sep="")),
                                                                   each = prod(factor_levels)/prod(factor_levels[1:j]),
                                                                   times = prod(factor_levels)/prod(factor_levels[j:factors])
    ))))
  }
  
  if(factors == 1){
    names(dataframe_means) <- c("mu","SD",factornames[1])
    dataframe_means[,factornames[1]] <- ordered(dataframe_means[,factornames[1]], levels = labelnameslist[[1]])
  }
  if(factors == 2){
    names(dataframe_means)<-c("mu","SD",factornames[1],factornames[2])
    dataframe_means[,factornames[1]] <- ordered(dataframe_means[,factornames[1]], levels = labelnameslist[[1]])
    dataframe_means[,factornames[2]] <- ordered(dataframe_means[,factornames[2]], levels = labelnameslist[[2]])
  }
  
  if(factors == 3) {
    names(dataframe_means) <- c("mu","SD",factornames[1],factornames[2],factornames[3])
    dataframe_means[,factornames[1]] <- ordered(dataframe_means[,factornames[1]], levels = labelnameslist[[1]])
    dataframe_means[,factornames[2]] <- ordered(dataframe_means[,factornames[2]], levels = labelnameslist[[2]])
    dataframe_means[,factornames[3]] <- ordered(dataframe_means[,factornames[3]], levels = labelnameslist[[3]])
  }
  
  if(factors == 1){meansplot = ggplot(dataframe_means, aes_string(y = "mu", x = factornames[1]))}
  if(factors == 2){meansplot = ggplot(dataframe_means, aes_string(y = "mu", x = factornames[1], colour = factornames[2]))}
  if(factors == 3){meansplot = ggplot(dataframe_means, aes_string(y = "mu", x = factornames[1], colour = factornames[2])) + facet_wrap(  paste("~",factornames[3],sep=""))}
  
  #Set custom color palette if factor 2 has a length greater than 8
  if (factors >= 2 && length(labelnameslist[[2]]) >= 9) {
    
    meansplot2 = meansplot +
      geom_point(position = position_dodge(width=0.9), shape = 10, size=5, stat="identity") + #Personal preference for sd -- ARC
      geom_errorbar(aes(ymin = mu-sd, ymax = mu+sd),
                    position = position_dodge(width=0.9), size=.6, width=.3) +
      coord_cartesian(ylim=c(min(mu)-sd, max(mu)+sd)) +
      theme_bw(base_size = 16) + ggtitle("Means for each condition in the design") +
      scale_colour_brewer(palette = "Dark2")
    
  } else {
    
    meansplot2 = meansplot +
      geom_point(position = position_dodge(width=0.9), shape = 10, size=5, stat="identity") + #Personal preference for sd -- ARC
      geom_errorbar(aes(ymin = mu-sd, ymax = mu+sd),
                    position = position_dodge(width=0.9), size=.6, width=.3) +
      coord_cartesian(ylim=c(min(mu)-sd, max(mu)+sd)) +
      theme_bw() + ggtitle("Means for each condition in the design") +
      scale_colour_brewer(palette = "Dark2")
  }
  if(plot == TRUE){
    print(meansplot2)  #should be blocked in Shiny context
  }
  
  # Return results in list()
  invisible(list(dataframe = dataframe,
                 design = design,
                 design_list = design_list,
                 factors = factors,
                 frml1 = frml1,
                 frml2 = frml2,
                 mu = mu,
                 sd = sd,
                 r = r,
                 n = n,
                 cor_mat = cor_mat,
                 sigmatrix = sigmatrix,
                 string = string,
                 labelnames = labelnameslist,
                 factornames = factornames,
                 meansplot = meansplot2))
}

#ANOVA power function; last update: May 17 2019
ANOVA_power <- function(design_result, alpha_level = 0.05, p_adjust = "none", nsims = 1000, seed = NULL){
  
  if (is.element(p_adjust, c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")) == FALSE ) {
    stop("p_adjust must be of an acceptable adjustment method: see ?p.adjust")
  }
  
  if (nsims < 10) {
    stop("The number of repetitions in simulation must be at least 10; suggested at least 1000 for accurate results")
  }
  
  
  # #Require necessary packages
  # requireNamespace(mvtnorm, quietly = TRUE)
  # requireNamespace(MASS, quietly = TRUE)
  # requireNamespace(afex, quietly = TRUE)
  # requireNamespace(emmeans, quietly = TRUE)
  # requireNamespace(ggplot2, quietly = TRUE)
  # requireNamespace(gridExtra, quietly = TRUE)
  # requireNamespace(reshape2, quietly = TRUE)
  
  options(scipen = 999) # 'turn off' scientific notation
  set.seed(seed)
  
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
  
  round_dig <- 4 #Set digits to which you want to round the output.
  
  if (missing(alpha_level)) {
    alpha_level <- 0.05
  }
  
  if (alpha_level >= 1 | alpha_level <= 0  ) {
    stop("alpha_level must be less than 1 and greater than zero")
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
  
  ###############
  # 2. Create Dataframe based on Design ----
  ###############
  
  #Count number of factors in design
  factors <- design_result$factors
  
  #Specify within/between factors in design: Factors that are within are 1, between 0
  design <- design_result$design
  
  sigmatrix <- design_result$sigmatrix
  
  #Create the data frame. This will be re-used in the simulation (y variable is overwritten) but created only once to save time in the simulation
  dataframe <- design_result$dataframe
  
  ###############
  # 3. Specify factors for formula ----
  ###############
  
  frml1 <- design_result$frml1
  frml2 <- design_result$frml2
  
  aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter formula 1 as designed above on the basis of the design
                                          data = dataframe, include_aov = FALSE,
                                          anova_table = list(es = "pes", p_adjust_method = p_adjust)) }) #This reports PES not GES
  
  
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
  
  #create empty dataframe to store simulation results
  #number of columns for ANOVA results and planned comparisons, times 2 (p-values and effect sizes)
  sim_data <- as.data.frame(matrix(
    ncol = 2 * (2 ^ factors - 1) + 2 * possible_pc,
    nrow = nsims
  ))
  
  paired_tests <- combn(unique(dataframe$cond),2)
  paired_p <- numeric(possible_pc)
  paired_d <- numeric(possible_pc)
  within_between <- sigmatrix[lower.tri(sigmatrix)] #based on whether correlation is 0 or not, we can determine if we should run a paired or independent t-test
  
  #Dynamically create names for the data we will store
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
  
  
  ###############
  # 7. Start Simulation ----
  ###############
  withProgress(message = 'Running simulations', value = 0, { #block outside of Shiny
  for (i in 1:nsims) { #for each simulated experiment
    incProgress(1/nsims, detail = paste("Now running simulation", i, "out of",nsims,"simulations")) #Block outside of Shiny
    #We simulate a new y variable, melt it in long format, and add it to the dataframe (surpressing messages)
    dataframe$y <- suppressMessages({
      melt(as.data.frame(mvrnorm(
        n = n,
        mu = mu,
        Sigma = as.matrix(sigmatrix)
      )))$value
    })
    
    # We perform the ANOVA using AFEX
    #Can be set to NICE to speed up, but required data grabbing from output the change.
    aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design
                                            data = dataframe, include_aov = FALSE, #Need development code to get aov_include function
                                            anova_table = list(es = "pes",
                                                               p_adjust_method = p_adjust))}) #This reports PES not GES
    
    for (j in 1:possible_pc) {
      x <- dataframe$y[which(dataframe$cond == paired_tests[1,j])]
      y <- dataframe$y[which(dataframe$cond == paired_tests[2,j])]
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
    sim_data[i,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                      aov_result$anova_table[[5]], #partial eta squared
                      p.adjust(paired_p, method = p_adjust), #p-values for paired comparisons, added correction for multiple comparisons
                      paired_d #effect sizes
    )}
  }) #close withProgress Block outside of Shiny
  
  ############################################
  #End Simulation              ###############
  
  
  ###############
  # 8. Plot Results ----
  ###############
  
  # melt the data into a long format for plots in ggplot2
  
  plotData <- suppressMessages(melt(sim_data[1:(2 ^ factors - 1)], value.name = 'p'))
  
  SalientLineColor <- "#535353"
  LineColor <- "Black"
  BackgroundColor <- "White"
  
  # plot each of the p-value distributions
  #create variable p to use in ggplot and prevent package check error.
  p <- plotData$p
  # Helper function for string wrapping.
  swr = function(string, nwrap=10) {
    paste(strwrap(string, width=10), collapse="\n")
  }
  swr = Vectorize(swr)
  
  # Create line breaks in variable
  plotData$variable = swr(chartr("_:", "  ", plotData$variable))
  
  plt1 = ggplot(plotData, aes(x = p)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1),
                       labels = seq(0, 1, by = .1)) +
    geom_histogram(colour = "black",
                   fill = "white",
                   breaks = seq(0, 1, by = .01)) +
    geom_vline(xintercept = alpha_level, colour = 'red') +
    facet_grid(variable ~ .) +
    labs(x = "p") +
    theme_bw()
  #Plot p-value distributions for simple comparisons
  # melt the data into a ggplot friendly 'long' format
  p_paired <- sim_data[(2 * (2 ^ factors - 1) + 1):(2 * (2 ^ factors - 1) + possible_pc)]
  
  plotData <- suppressMessages(melt(p_paired, value.name = 'p'))
  #create variable p to use in ggplot and prevent package check error.
  p <- plotData$p
  # Create line breaks in variable
  plotData$variable = swr(chartr("_:", "  ", plotData$variable))
  
  # plot each of the p-value distributions
  plt2 = ggplot(plotData, aes(x = p)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1),
                       labels = seq(0, 1, by = .1)) +
    geom_histogram(colour = "black",
                   fill = "white",
                   breaks = seq(0, 1, by = .01)) +
    geom_vline(xintercept = alpha_level, colour = 'red') +
    facet_grid(variable ~ .) +
    labs(x = expression(p)) +
    theme_bw()
  ###############
  # 9. Sumary of power and effect sizes of main effects and contrasts ----
  ###############
  
  #Main effects and interactions from the ANOVA
  power = as.data.frame(apply(as.matrix(sim_data[(1:(2 ^ factors - 1))]), 2,
                              function(x) round(mean(ifelse(x < alpha_level, 1, 0) * 100),round_dig)))
  
  es = as.data.frame(apply(as.matrix(sim_data[((2^factors):(2 * (2 ^ factors - 1)))]), 2,
                           function(x) round(median(x),round_dig)))
  
  main_results <- data.frame(power,es)
  names(main_results) = c("power","effect_size")
  
  
  
  #Data summary for pairwise comparisons
  power_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + 1):(2 * (2 ^ factors - 1) + possible_pc)]), 2,
                                     function(x) round(mean(ifelse(x < alpha_level, 1, 0) * 100),round_dig)))
  
  es_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + possible_pc + 1):(2*(2 ^ factors - 1) + 2 * possible_pc)]), 2,
                                  function(x) round(mean(x),round_dig)))
  
  pc_results <- data.frame(power_paired, es_paired)
  names(pc_results) = c("power","effect_size")
  
  #######################
  # Return Results ---- DEPRECATED
  #######################
  #if(verbose == TRUE){
  #  # The section below should be blocked out when in Shiny
  #  cat("Power and Effect sizes for ANOVA tests")
  #  cat("\n")
  #  print(main_results)
  #  cat("\n")
  #  cat("Power and Effect sizes for contrasts")
  #  cat("\n")
  #  print(pc_results)
  #}
  
  # Return results in list()
  invisible(list(sim_data = sim_data,
                 main_results = main_results,
                 pc_results = pc_results,
                 plot1 = plt1,
                 plot2 = plt2,
                 p_adjust = p_adjust,
                 nsims = nsims,
                 alpha_level = alpha_level))
}

# Define User Interface for simulations
ui <- fluidPage(
  titlePanel("ANOVA Simulation"),
  
  #Panel to define ANOVA design
  column(4, wellPanel(  
    
    h4("This is an app to calculate power for ANOVA designs through simulation (", a("R Package", href = "https://github.com/Lakens/ANOVApower"),
       ").  It is made by ", 
       a("Aaron Caldwell", href = "https://twitter.com/ExPhysStudent"), "and ", 
       a("Daniel Lakens", href = "https://twitter.com/Lakens"),"and we appreciate hearing any feedback you have as we develop this app."),
    
    h4("Add numbers for each factor that specify the number of levels in the factors (e.g., 2 for a factor with 2 levels). Add a 'w' after the number for within factors, and a 'b' for between factors. Seperate factors with a * (asterisks). Thus '2b*3w' is a design with two factors, the first of which has 2 between levels, and the second of which has 3 within levels."),
    
    textInput(inputId = "design", label = "Design Input",
              value = "2b*2w"),
    
    h4("Specify one word for each factor (e.g., AGE and SPEED) and the level of each factor (e.g., old and yound for a factor age with 2 levels)."),
    
    textInput("labelnames", label = "Factor & level labels",
              value = "AGE,old,young,SPEED,fast,slow"),
    
    sliderInput("sample_size",
                label = "Sample Size per Cell",
                min = 3, max = 250, value = 80),
    
    textInput(inputId = "sd", label = "Standard Deviation",
              value = 1.03),
    
    h4("Specify the correlation for within subjects factors."),
    
    sliderInput("r",
                label = "Correlation",
                min = 0, max = 1, value = 0.87),
    
    h4("Note that for each cell in the design, a mean must be provided. Thus, for a '2b*3w' design, 6 means need to be entered. Means need to be entered in the correct order. The app provides a plot so you can check if you entered means correctly. The general principle has designated factors (i.e., AGE and SPEED) and levels (e.g., old, young)."),
    
    textInput("mu", label = "Vector of Means", 
              value = "1.03, 1.21, 0.98, 1.01"),
    
    #Button to initiate the design
    h4("Click the button below to set up the design - Check the output to see if the design is as you intended, then you can run the simulation."),
    
    actionButton("designBut","Set-Up Design"),
    
    #Conditional; once design is clicked. Then settings for power simulation can be defined
    conditionalPanel("input.designBut >= 1",   
    selectInput("p_adjust", label = "Adjustment for multiple comparisons", 
                choices = list("None" = "none", "Holm-Bonferroni" = "holm",
                               "Bonferroni" = "bonferroni",
                               "False Discovery Rate" = "fdr"), selected = 1),
    # Everyone sets the default seed to 42 I always pick Jean Valjean, this can be changed to anything
    # Also the min and max are the largest and smallest values that R will take for setting a seed - WKH
    numericInput(inputId = 'setSeedValue', label = "Set Simulation Seed", 24601, min = -2147483647, max = 2147483647),
    
  
    

                     
                     
                     sliderInput("sig",
                                 label = "Alpha Level",
                                 min = 0, max = 1, value = 0.05),
                     h4("To test out the app, keep the number of simulations to 100. To get more accurate results, increase the number of simulations."),
                     sliderInput("nsims", 
                                 label = "Number of Simulations",
                                 min = 100, max = 10000, value = 100, step = 100),
                     h4("Click either button below to start the simulation"),
                     actionButton("sim", "Print Results of Simulation")
    ),
    
    conditionalPanel("input.sim >=1",
                     downloadButton("report", "Download Report")
    )
    
    )),
  
  
  #Output for Design
  column(5,  
         conditionalPanel("input.designBut >= 1", 
                          h3("Design for Simulation")),
         
         verbatimTextOutput("DESIGN"),
         
         plotOutput('plot'),
         
         tableOutput("corMat")),
  #output for Simulation
  column(4, 
         conditionalPanel("input.sim >= 1", 
                          h3("Simulation Results")),
         
         tableOutput('tableMain'),
         
         tableOutput('tablePC'),
         
         verbatimTextOutput("p_adjust")
  )
)



# Define server logic 

server <- function(input, output) {
  
  #Create set of reactive values
  values <- reactiveValues(design_result = 0, power_result = 0)
  
  #Produce ANOVA design
  observeEvent(input$designBut, {values$design_result <- ANOVA_design(string = as.character(input$design),
                                                                      n = as.numeric(input$sample_size), 
                                                                      mu = as.numeric(unlist(strsplit(input$mu, ","))), 
                                                                      labelnames = as.vector(unlist(strsplit(gsub("[[:space:]]", "",input$labelnames), ","))), 
                                                                      sd = as.numeric(input$sd), 
                                                                      r = as.numeric(input$r), 
                                                                      plot = FALSE)
  })
  
  
  #Output text for ANOVA design
  output$DESIGN <- renderText({
    req(input$designBut)
    
    paste("The design is set as", values$design_result$string, 
          " 
          ", 
          "Model formula: ", deparse(values$design_result$frml1), 
          " 
          ",
          "Sample size per cell n = ", values$design_result$n)
    })
  
  #Output of correlation and standard deviation matrix
  output$corMat <- renderTable(colnames = FALSE, 
                               caption = "Covariance Matrix",
                               caption.placement = getOption("xtable.caption.placement", "top"),
                               {
                                 req(input$designBut)
                                 values$design_result$sigmatrix
                                 
                               })
  #Output plot of the design
  output$plot <- renderPlot({
    req(input$designBut)
    values$design_result$meansplot})
  
  
  
  #Runs simulation and saves result as reactive value
  observeEvent(input$sim, {values$power_result <- ANOVA_power(values$design_result, 
                                                              alpha_level = input$sig,
                                                              nsims = input$nsims,
                                                              p_adjust = as.character(input$p_adjust),
                                                              seed = input$setSeedValue)
  
  
  })
  
  #Table output of ANOVA level effects; rownames needed
  output$tableMain <-  renderTable({
    req(input$sim)
    values$power_result$main_results},
    rownames = TRUE)
  
  #Table output of pairwise comparisons; rownames needed
  output$tablePC <-  renderTable({
    req(input$sim)
    values$power_result$pc_result},
    rownames = TRUE)
  
  output$p_adjust <- renderText({
    req(input$sim)
    
    paste("Adjustment for multiple comparisons", values$power_result$p_adjust)
  })
  
  #Create downloadable report in markdown TINYTEX NEEDS TO BE INSTALLED 
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(tablePC = values$power_result$pc_result,
                     tableMain = values$power_result$main_results,
                     pvalue_plot1 = values$power_result$plot1,
                     pvalue_plot2 = values$power_result$plot2,
                     means_plot = values$design_result$meansplot,
                     n = values$design_result$n,
                     padjust = values$power_result$p_adjust,
                     model = deparse(values$design_result$frml1),
                     design = values$design_result$string,
                     cor_mat = values$design_result$cor_mat,
                     sigmatrix = values$design_result$sigmatrix,
                     seed_number = input$setSeedValue,
                     nsims = values$power_result$nsims,
                     alpha_level = values$power_result$alpha_level)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}

shinyApp(ui, server)