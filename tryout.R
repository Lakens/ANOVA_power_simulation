options(scipen=999)

library(mvtnorm)
library(afex)
library(lsmeans)
library(ggplot2)
library(psych)
library(tidyr)
require(gridExtra)
require(reshape2)

#String used to specify the design
# e.g., "2" for 1 factor, "2*2*2" for three factors
string <- "2b*2w" #String used to specify the design
factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))

as.list(strsplit(string, ("[^A-Za-z]+")))

design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]]
design <- rep(design, as.list(strsplit(string, "\\D+")[[1]]))
design <- as.numeric(design == "w") #if within design, set value to 0, otherwise to 1

design*design


help("regmatches")
if(factors == 1){frml1 <- as.formula("y ~ a + Error(subject/a)")}
if(factors == 1){frml2 <- as.formula("~a")}
if(factors == 2){frml1 <- as.formula("y ~ a*b + Error(subject/a*b)")}
if(factors == 2){frml2 <- as.formula("~a+b")}
if(factors == 3){frml1 <- as.formula("y ~ a*b*c + Error(subject/a*b*c)")}
if(factors == 3){frml2 <- as.formula("~a+b+c")}
if(factors == 4){frml1 <- as.formula("y ~ a*b*c*d + Error(subject/a*b*c*d)")}
if(factors == 4){frml2 <- as.formula("~a+b+c+d")}

#indicate which adjustment for multiple comparisons you want to use (e.g., "holm")
p_adjust <- "none" 

# specify means. 
#for 1x2: c(1, 2) so 2 means
#for 2x2: c(1, 2, 3, 4) so 4 means
#for 2x2x2: c(1, 2, 3, 4, 5, 6, 7, 8) so 8 means
mu = c(1, 2, 2, 2) # population means - should match up with the design
sd=1 #population standard deviations
r=0.5 # correlation between repeated measures
n<-500 #number of subjects
nsims = 100 # how many simulation replicates?

#create matrix
sigmatrix <- matrix(r, length(mu),length(mu)) #create a matrix filled with value of correlation, nrow and ncol set to length in mu
diag(sigmatrix) <- sd # replace the diagonal with the sd

df <- as.data.frame(rmvnorm(n=n,
                            mean=mu,
                            sigma=sigmatrix))
str(df)
colMeans(df)

cor(df)






design
sigma=matrix(c(sd,r,r,r,r,r,r,r,
               r,sd,r,r,r,r,r,r,
               r,r,sd,r,r,r,r,r,
               r,r,r,sd,r,r,r,r,
               r,r,r,r,sd,r,r,r,
               r,r,r,r,r,sd,r,r,
               r,r,r,r,r,r,sd,r,
               r,r,r,r,r,r,r,sd), 8,8)

sss <- matrix(NA, length(mu),length(mu))
diag(sss) <- sd
sss[1:2,1:2] <- matrix(design, 2,2)





nm[] <- mydata[3,3]



#create new matrix
mydata <- data.frame(Var1 = c("A", "A", "B"), Var2 = c("B", "C", "C"), values = c(2, 3, 6))
vals<-sort(unique(c(as.character(mydata$Var1), as.character(mydata$Var2))))
nm<-matrix(NA, nrow=length(vals), ncol=length(vals), dimnames=list(vals, vals))
diag(nm)<-1

#fill 
nm[as.matrix(mydata[, 1:2])] <- mydata[,3]

#symmetric
nm[lower.tri(nm)] <- nm[upper.tri(nm)]
nm



