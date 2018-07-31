#' Miscellaneous helpers
#'
#' Bootstrapped standard error of the median
#' @export

bootMedianSE <- function(v,nsim=1000){
	bootmedian <- function(v,i) median(v[i],na.rm=T)
	bootRes <- boot::boot(v,bootmedian,nsim)
	return(sd(bootRes$t,na.rm=T))
}