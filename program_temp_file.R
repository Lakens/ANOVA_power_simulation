design_result <- ANOVA_design(string = "2b*2w",
                              n = 80, 
                              mu = c(1.03, 1.21, 0.98, 1.01), 
                              sd = 1.03, 
                              r=0.87, 
                              p_adjust = "none",
                              labelnames = c("age", "old", "young", "speed", "fast", "slow"))

alpha <- 0.05
nsims <- 100


design_result$factornames

string <- "3b*3w"

string <- "3b*3w*2b"

factors <- 2

labelnames = c("age", "old", "young", "speed", "fast", "slow")

labelnames = c("age", "old", "young", "medium", "speed", "fast", "slow", "medium")

labelnames = c("age", "old", "young", "medium", "speed", "fast", "slow", "medium", "country", "NL", "US")


labelnames <- design_result$labelnames

labelnames1 <- labelnames[(1 + 1):(1+as.numeric(strsplit(string, "\\D+")[[1]])[1])]
if(factors > 1){labelnames2 <- labelnames[(as.numeric(strsplit(string, "\\D+")[[1]])[1] + 3):((as.numeric(strsplit(string, "\\D+")[[1]])[1] + 3) + as.numeric(strsplit(string, "\\D+")[[1]])[2] - 1)]}
if(factors > 2){labelnames3 <- labelnames[(as.numeric(strsplit(string, "\\D+")[[1]])[2] + as.numeric(strsplit(string, "\\D+")[[1]])[1] + 4):((as.numeric(strsplit(string, "\\D+")[[1]])[2] + as.numeric(strsplit(string, "\\D+")[[1]])[1] + 4) + as.numeric(strsplit(string, "\\D+")[[1]])[3] - 1)]}

factornames1 <- labelnames[1]
if(factors > 1){factornames2 <- labelnames[as.numeric(strsplit(string, "\\D+")[[1]])[1] + 2]}
if(factors > 2){factornames3 <- labelnames[as.numeric(strsplit(string, "\\D+")[[1]])[2] + as.numeric(strsplit(string, "\\D+")[[1]])[1] + 3]}

if(factors == 1){labelnames <- list(labelnames1)}
if(factors == 2){labelnames <- list(labelnames1,labelnames2)}
if(factors == 3){labelnames <- list(labelnames1,labelnames2,labelnames3)}

if(factors == 1){factornames <- c(factornames1)}
if(factors == 2){factornames <- c(factornames1,factornames2)}
if(factors == 3){factornames <- c(factornames1,factornames2,factornames3)}


labelnames1
labelnames2
labelnames3
factornames1
factornames2
factornames3




xx <- aov_result$anova_table


yy <- aov_result$anova_table

row.names(aov_result$anova_table) <- c("1","2","3")

row.names(aov_result$anova_table) <- factornames}

xxxx <- design_result$factornames
xxxx[2]


paste(design_result$factornames[1],"_",design_result$factornames[2], sep = "")

if(factors == 1){row.names(main_results) <- factornames[1]}
if(factors == 2){row.names(main_results) <- c(factornames[1],
                                              factornames[2],
                                              paste(design_result$factornames[1],
                                                    "_",
                                                    design_result$factornames[2],
                                                    sep = "")}
if(factors == 3){row.names(main_results) <- c(factornames[1],
                                              factornames[2],
                                              factornames[3],
                                              paste(design_result$factornames[1],
                                                    "_",
                                                    design_result$factornames[2], 
                                                    sep = ""),
                                              paste(design_result$factornames[1],
                                                    "_",
                                                    design_result$factornames[3],
                                                    sep = ""),
                                              paste(design_result$factornames[2],
                                                    "_",
                                                    design_result$factornames[3],
                                                    sep = ""),
                                              paste(design_result$factornames[1],
                                                    "_",
                                                    design_result$factornames[2],
                                                    "_",
                                                    design_result$factornames[3],
                                                    sep = "")}




paste("y ~ a + Error(subject/a)",sep="")
as.formula("y ~ a + Error(subject/a)")
as.formula(paste("y ~ ",design_result$factornames[1]," + Error(subject/",design_result$factornames[1],")",sep=""))

str(xxx)







#one factor
#if(factors == 1 & sum(design) == 1){frml1 <- as.formula("y ~ a + Error(subject/a)")}
if(factors == 1 & sum(design) == 1){frml1 <- as.formula(paste("y ~ ",factornames[1]," + Error(subject/",factornames[1],")",sep=""))}

#if(factors == 1 & sum(design) == 0){frml1 <- as.formula("y ~ a + Error(1 | subject)")}
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
