#' Miscellaneous helpers
#'
#' Bootstrapped 95% CI (quantile method)
#' @export

bootMeanCI <- function(v,nsim=1000,...){
	bootmean <- function(v,i) mean(v[i],na.rm=T,...)
	bootRes <- boot::boot(v,bootmean,nsim)
	return(c(quantile(bootRes$t, probs = 0.025,na.rm=T),quantile(bootRes$t, probs = 0.975,na.rm=T)))
}