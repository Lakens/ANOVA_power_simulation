options(scipen=999)

library(mvtnorm)
library(afex)
library(lsmeans)
library(ggplot2)
library(psych)
library(tidyr)
require(gridExtra)
require(reshape2)
require(sjstats)

#String used to specify the design
# e.g., "2" for 1 factor, "2*2*2" for three factors
string <- "3b*3w" #String used to specify the design
factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))

#Specify within/between factors in design: Factors that are within are 1, between 0
design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]]
design <- as.numeric(design == "w") #if within design, set value to 1, otherwise to 0


#indicate which adjustment for multiple comparisons you want to use (e.g., "holm")
p_adjust <- "none" 

# specify means. 
#for 1x2: c(1, 2) so 2 means
#for 2x2: c(1, 2, 3, 4) so 4 means
#for 2x2x2: c(1, 2, 3, 4, 5, 6, 7, 8) so 8 means

#2x2
#mu = c(1, 2, 2, 2) # population means - should match up with the design
#2x2x2
#mu = c(1, 2, 2, 2, 1, 2, 2, 1) # population means - should match up with the design
#3x3
mu = c(1, 2, 2, 2, 1, 2, 2, 1, 2) # population means - should match up with the design
#3x3x3
#mu = c(2, 1, 2, 2, 2, 1, 2, 2, 1, 2, 2, 3, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 2, 2, 2) # population means - should match up with the design


if(prod(as.numeric(strsplit(string, "\\D+")[[1]])) != length(mu)){stop("the length of the vector with means does not match the study design")}

sd=1 #population standard deviations
r=0.5 # correlation between repeated measures
n<-5 #number of subjects
nsims = 100 # how many simulation replicates?

# create temp matrix to create one dataset, to get design ################################
# (this section is lazy but efficient programming)
sigmatrix <- matrix(r, length(mu),length(mu)) #create a matrix filled with value of correlation, nrow and ncol set to length in mu
diag(sigmatrix) <- sd # replace the diagonal with the sd

df <- as.data.frame(rmvnorm(n=n,
                            mean=mu,
                            sigma=sigmatrix))
df$subject<-as.factor(c(1:n))
df <- melt(df, 
           id.vars = "subject", 
           variable.name = "cond",
           value.name = "y")
for(j in 1:factors){
  df <- cbind(df, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                     1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                     sep="")), 
                                       each = n*prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                       times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}
names(df)[4:(3+factors)] <- letters[1:factors]

############################################
#Specify factors for formula ###############

#one factor
if(factors == 1 & sum(design) == 1){frml1 <- as.formula("y ~ a + Error(subject/a)")}
if(factors == 1 & sum(design) == 0){frml1 <- as.formula("y ~ a")}

if(factors == 2){
  if(sum(design) == 2){frml1 <- as.formula("y ~ a*b + Error(subject/a*b)")}
  if(sum(design) == 0){frml1 <- as.formula("y ~ a*b")}
  if(all(design == c(1, 0)) == TRUE){frml1 <- as.formula("y ~ a*b + Error(subject/a)")}
  if(all(design == c(0, 1)) == TRUE){frml1 <- as.formula("y ~ a*b + Error(subject/b)")}
}

if(factors == 3){
  if(sum(design) == 3){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*b*c)")}
  if(sum(design) == 0){frml1 <- as.formula("y ~ a*b*c")}
  if(all(design == c(1, 0, 0)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/a)")}
  if(all(design == c(0, 1, 0)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/b)")}
  if(all(design == c(0, 0, 1)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/c)")}
  if(all(design == c(1, 1, 0)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*b)")}
  if(all(design == c(0, 1, 1)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/b*c)")}
  if(all(design == c(1, 0, 1)) == TRUE){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*c)")}
}

#Specify second formula used for plotting
if(factors == 1){frml2 <- as.formula("~a")}
if(factors == 2){frml2 <- as.formula("~a+b")}
if(factors == 3){frml2 <- as.formula("~a+b+c")}

############################################
#Specify factors for formula ###############

if(factors == 1){
  design_list <- unique(paste(df$a, sep = "")) #hardcoded, limits total # factors
}
if(factors == 2){
  design_list <- unique(paste(df$a,df$b, sep = "")) #hardcoded, limits total # factors
}
if(factors == 3){
  design_list <- unique(paste(df$a,df$b,df$c, sep = "")) #hardcoded, limits total # factors
}

############################################
#Create Real Covariance Matrix##############

#Create empty matrix
sigmatrix <- data.frame(matrix(ncol=length(mu), nrow = length(mu)))

# General approach: For each factor in the list of the design, save the first item (e.g., a1b1)
# Then for each factor in the design, if 1, set number to wildcard
for(i1 in 1:length(design_list)){
  current_factor <- design_list[i1]
  for(i2 in 1:length(design)){
    #We set each number that is within to a wildcard, so that all within subject factors are matched
    if(design[i2] == 1){substr(current_factor, i2*2,  i2*2) <- "*"} 
  }
  sigmatrix[i1,]<-as.numeric(grepl(current_factor, design_list)) # compare factors that match with current factor, given wildcard, save list to sigmatrix
}

frml1 <- as.formula("y ~ a*b + Error(subject)")
# We perform the ANOVA using AFEX
within.aov<-aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                    data=df) #This reports PES not GES


within.aov<-aov_ez("subject", "y",  #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                    data=df, between = c("a"), within = c("b")) #This reports PES not GES



within.aov<-aov_ez("subject", "y",  #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                   data=df, between = c("a"), within = c("b")) #This reports PES not GES



#Get columns that specify factors, but only when within (marked as 1 in design)
temp <- df[, c(FALSE,FALSE,FALSE,as.logical(design))]
#Get number of unique within conditions based on design.
#add this should only be done when more than 1 column - now gives an error
within_factors <- unique(apply((temp)[1:(factors)], 1, paste, collapse="")) #add this should only be done when more than 1 column
within_factors







#String used to specify the design
# e.g., "2" for 1 factor, "2*2*2" for three factors
string <- "3b*3b*3b" #String used to specify the design
factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))

#Specify within/between factors in design: Factors that are within are 1, between 0
design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]]
design <- as.numeric(design == "w") #if within design, set value to 1, otherwise to 0


#Subject list is created counting from last to first factor
#if w: repeat current string
#id b: repeat current string, add current factor

subject <- 1:n #Set minimal subject to 1
j2<-3
j3<-2
for(j2 in length(design):1){ #for each factor in the design, from last to first
  #if w: repeat current string as often as the levels in the current factor (e.g., 3)
  #id b: repeat current string + max of current subject
  if(design[j2] == 1){subject <- rep(subject,as.numeric(strsplit(string, "\\D+")[[1]])[j2])}
  subject_length <- length(subject) #store current length - to append to string of this length below
  if(design[j2] == 0){
    for(j3 in 2:as.numeric(strsplit(string, "\\D+")[[1]])[j2]){
      subject <- append(subject,subject[1:subject_length]+max(subject))
    }
  }
}

subject






#all between, we add the correct subject numbers
df$subject<-as.factor(c(1:(n*prod(as.numeric(strsplit(string, "\\D+")[[1]])))))
df$subject<-as.factor(c(1:(n*length(wihin_factors))))


highest_n <- prod(as.numeric(strsplit(string, "\\D+")[[1]]))/length(within_factors)


n*prod(as.numeric(strsplit(string, "\\D+")[[1]]))


apply(df[3:5], 1, paste, collapse="")

design_list <- unique(apply((df)[4:(3+factors)], 1, paste, collapse=""))


tempy2 <- unique(paste(tempy, sep = "")) 

factors
d

as.numeric(strsplit(string, "\\D+")[[1]])

design_list <- unique(paste(df[4:5],sep = "")) 



