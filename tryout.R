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
string <- "2w*2b" #String used to specify the design
factors <- length(as.numeric(strsplit(string, "\\D+")[[1]]))

design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]]
design <- rep(design, as.list(strsplit(string, "\\D+")[[1]]))
design <- as.numeric(design == "w") #if within design, set value to 0, otherwise to 1

design*design*factors


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
#order = a1b1,a1b2,a1b3,a2b1,a2b2,a2b3

mu = c(1, 2, 3, 4, 5, 6, 7, 8) # population means - should match up with the design
sd=1 #population standard deviations
r=0.5 # correlation between repeated measures
n<-5 #number of subjects
nsims = 100 # how many simulation replicates?

#create matrix
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
  # Let's break this down - it's a bit tricky. First, we want to create a list of a1 a2 b1 b2 that will indicate the factors. 
  # We are looping this over the number of factors.
  # This: as.numeric(strsplit(string, "\\D+")[[1]]) - takes the string used to specify the design and turn it in a list. 
  # we take the letters from the alfabet: paste(letters[[j]] and add numbers 1 to however many factors there as: 1:as.numeric(strsplit(string, "\\D+")[[1]])[j], sep="")
  # We this get e.g. ,a1 a2 - we repeat these each: n*(2^(factors-1)*2)/(2^j) and them times:  (2^j/2) to get a list for each factor
  # We then bind these together with the existing dataframe.
  df <- cbind(df, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                     1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                     sep="")), 
                                       each = n*prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                       times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}
names(df)[4:(3+factors)] <- letters[1:factors]
# We perform the ANOVA using AFEX
within.aov<-aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design 
                    data=df,
                    anova_table = list(es = "pes", p_adjust_method = p_adjust)) #This reports PES not GES

emmip(within.aov, a~b)


design
sigma=matrix(c(sd,r,r,r,r,r,r,r,
               r,sd,r,r,r,r,r,r,
               r,r,sd,r,r,r,r,r,
               r,r,r,sd,r,r,r,r,
               r,r,r,r,sd,r,r,r,
               r,r,r,r,r,sd,r,r,
               r,r,r,r,r,r,sd,r,
               r,r,r,r,r,r,r,sd), 8,8)

sss <- matrix(r, length(mu),length(mu))


k=2
for(k in 1:factors){
  sss[(sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k])-sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k-1])):sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k]),
      (sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k])-sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k-1])):sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k])] <- matrix(design[k], 
                                                          (as.numeric(strsplit(string, "\\D+")[[1]])[[k]]), 
                                                          (as.numeric(strsplit(string, "\\D+")[[1]])[[k]]))
}

#Factors that are within should be 1, between 0
design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]]
design <- as.numeric(design == "w") #if within design, set value to 1, otherwise to 0

#Get list of complete design
ppp <- unique(paste(df$a,df$b, sep = ""))
str(ppp)
#Create empty dataframe
df_r <- data.frame(matrix(ncol=length(mu), nrow = length(mu)))
str(df_r)
#Factors that are within should be 1, between 0
design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]]
design <- as.numeric(design == "w") #if within design, set value to 1, otherwise to 0
str(design)

for(i1 in 1:length(ppp)){
  zzz <- ppp[i1]
  for(i2 in 1:length(design)){
    if(design[i2]==1){substr(zzz, i2*2,  i2*2) <- "*"}
  }
  df_r[i1,]<-as.numeric(grepl(zzz, ppp))
}
str(df_r)

zxc<-glob2rx("a*b1")
df_r[1,]<-grepl(zxc, ppp)
zxc<-glob2rx("a*b2")
df_r[2,]<-grepl(zxc, ppp)
zxc<-glob2rx("a*b3")
df_r[3,]<-grepl(zxc, ppp)

str(ppp)

x1<-"a"
x2<-"*"
x3<-"b"
x4<-"1"
zxy <- paste(x1,x2,x3,x4, sep="")
df_r[1,]<-grepl(zxc, ppp)





zzz <- as.list(unique(paste(df$a)))

as.list(ppp[1])*design

as.list(ppp[1])


zxc<-glob2rx(ppp[1])
df_r[1,]<-grepl(zxc, ppp)


ll<-levels(df$a)
ll



ppp$Var1==ppp$Var2