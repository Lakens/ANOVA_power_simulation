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
string <- "3b*3w" #String used to specify the design
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

mu = c(1, 2, 3, 4, 5, 6, 7, 8, 9) # population means - should match up with the design
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
str(df)
colMeans(df)

cor(df)

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

mean(as.numeric(strsplit(design, "\\D+")[[1]]))

if(mean(design)==0){
  sss <- matrix(0, length(mu),length(mu))
} else if(mean(design)==1){
  sss <- matrix(r, length(mu),length(mu))
} else {
  
}


k=2
for(k in 1:factors){
  sss[(sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k])-sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k-1])):sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k]),
      (sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k])-sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k-1])):sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k])] <- matrix(design[k], 
                                                          (as.numeric(strsplit(string, "\\D+")[[1]])[[k]]), 
                                                          (as.numeric(strsplit(string, "\\D+")[[1]])[[k]]))
}

k=1
sum(as.list(strsplit(string, "\\D+"))[[1:k]]))-1

k=2
sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k])-1

diag(sss) <- sd


matrix(design[k], (as.numeric(strsplit(string, "\\D+")[[1]])[[k]]), (as.numeric(strsplit(string, "\\D+")[[1]])[[k]]))

matrix(0,2,2)

x<-(as.list(strsplit(string, "\\D+")[[1]])[[k]])
str(x)
as.list(strsplit(string, "\\D+")[[1]])[k]


sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k])-sum(as.numeric(strsplit(string, "\\D+")[[1]])[1:k-1])

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




x <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]][1]
y <- (1:(as.numeric(strsplit(string, "\\D+")[[1]]))[1])
d1 <- expand.grid(x = x, y = y)
x <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]][2]
y <- (1:(as.numeric(strsplit(string, "\\D+")[[1]]))[2])
d2 <- expand.grid(x = x, y = y)
str(d2)
print(d1)

d3<- expand.grid(x = d1, y = d2)


 

#Factors that are within should be 1, between 0
design <- strsplit(gsub("[^A-Za-z]","",string),"",fixed=TRUE)[[1]]
design <- as.numeric(design == "w") #if within design, set value to 1, otherwise to 0

#Get list of complete design
ppp <- unique(paste(df$a,df$b, sep = ""))
#Create empty dataframe
df_r <- data.frame(matrix(ncol=length(mu), nrow = length(mu)))
zxc<-glob2rx("a*b1")
df_r[1,]<-grepl(zxc, ppp)
zxc<-glob2rx("a*b2")
df_r[2,]<-grepl(zxc, ppp)
zxc<-glob2rx("a*b3")
df_r[3,]<-grepl(zxc, ppp)

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