#' Miscellaneous helpers
#'
#' BootstrappedSEM
#' @export

bootMeanSE <- function(v,nsim=1000,...){
	bootmean <- function(v,i) mean(v[i],na.rm=T,...)
	bootRes <- boot::boot(v,bootmean,nsim)
	return(sd(bootRes$t,na.rm=T))
}