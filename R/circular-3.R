#' Circular data - helpers
#'
#' Boostrapped standard error of angular mean
#' @export

bootAngMeanSE <- function(v,nsim=1000){
	bootmean <- function(v,i) angMean(v[i])
	bootRes <- boot::boot(v,bootmean,nsim)
	return(sd(bootRes$t,na.rm=T))
}
