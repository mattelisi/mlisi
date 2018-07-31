#' Miscellaneous helpers
#'
#' Compute 95% CI (assume Gaussian random variable)
#' @export

ci <- function(v){
	qt(0.975, df = length(v) -1)*sqrt(var(v,na.rm=T)/length(v))
}