#Create plot of means to vizualize the design

df_means <- data.frame(mu, SE = sd / sqrt(n))
for(j in 1:factors){
  df_means <- cbind(df_means, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                     1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                     sep="")), 
                                       each = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                       times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}

if(factors == 1){names(df_means)<-c("mu","SE","a")}
if(factors == 2){names(df_means)<-c("mu","SE","a","b")}
if(factors == 3){names(df_means)<-c("mu","SE","a","b","c")}

if(factors == 1){meansplot = ggplot(df_means, aes(y = mu, x = a))}
if(factors == 2){meansplot = ggplot(df_means, aes(y = mu, x = a, fill=b))}
if(factors == 3){meansplot = ggplot(df_means, aes(y = mu, x = a, fill=b)) + facet_wrap(  ~ c)}

meansplot = meansplot +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin = mu-SE, ymax = mu+SE), 
                position = position_dodge(width=0.9), size=.6, width=.3) +
  coord_cartesian(ylim=c((.7*min(mu)), 1.2*max(mu))) +
  theme_bw()
meansplot

#For one factor
df_means <- data.frame(mu, SE = sd / sqrt(n))
for(j in 1:factors){
  df_means <- cbind(df_means, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                                 1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                                 sep="")), 
                                                   each = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                                   times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}
names(df_means)<-c("mu","SE","a")
plt1 = ggplot(df_means, aes(y = mu, x = a)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin = mu-SE, ymax = mu+SE), 
                position = position_dodge(width=0.9), size=.6, width=.3) +
  coord_cartesian(ylim=c((.7*min(mu)), 1.2*max(mu))) +
  theme_bw()
plt1



#For two factors
df_means <- data.frame(mu, SE = sd / sqrt(n))
for(j in 1:factors){
  df_means <- cbind(df_means, as.factor(unlist(rep(as.list(paste(letters[[j]], 
                                                                 1:as.numeric(strsplit(string, "\\D+")[[1]])[j], 
                                                                 sep="")), 
                                                   each = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[1:j]),
                                                   times = prod(as.numeric(strsplit(string, "\\D+")[[1]]))/prod(as.numeric(strsplit(string, "\\D+")[[1]])[j:factors])
  ))))
}
names(df_means)<-c("mu","SE","a","b", "c")
plt1 = ggplot(df_means, aes(y = mu, x = a, fill=b)) +
  facet_wrap(  ~ c) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin = mu-SE, ymax = mu+SE), 
                position = position_dodge(width=0.9), size=.6, width=.3) +
  coord_cartesian(ylim=c((.7*min(mu)), 1.2*max(mu))) +
  theme_bw()
plt1






frml2
## add univariate CIs:
emmip(within.aov, a ~ b , CIs = TRUE)
