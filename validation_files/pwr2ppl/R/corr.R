#'Compute power for Pearson's Correlation
#'Takes correlation and range of values
#'@param r Correlation
#'@param nlow Starting sample size
#'@param nhigh Ending sample size
#'@param by Incremental increase in sample size from low ot high
#'@param tails one or two-tailed tests (default is 2)
#'@param alpha Type I error (default is .05)
#'@return Power for Pearson's Correlation
#'@export
#'
#'
corr<-function(r,nlow, nhigh, alpha=.05, tails=2, by=1)
{
  d<-abs(2*abs(r))/(1-r^2)^.5
  for(n in seq(nlow,nhigh, by)){
    delta<-(d*(n-2)^.5)/2
    alphatails<-alpha/tails
    tabled<-stats::qt(1-alphatails, df=n-2)
    t<-1-stats::pt(alphatails, 1, n-2)
    Power<-round(1-stats::pt(tabled, n-2,delta),4)
    print(paste("Power for n of", n, "=", Power))}
}
