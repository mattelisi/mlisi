#' Signal processing
#'
#' Normalize epochs with respect to baseline (e.g. useful for pupil size)
#' @param X matrix with one row for each trial
#' @param baseLength: n of baseline samples (counted from the left)
#' @export

baselineNorm<-function(X,baseLength=30){
 for (i in 1:dim(X)[1]){
  actBase<-mean(X[i,1:baseLength])
  X[i,]<-X[i,]-actBase
 }
 return(X)
}
 
