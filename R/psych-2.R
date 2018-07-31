#' Psychometric
#'
#' simple (negative) log-likelihood calculation for 1 condition
#' @param p parameter vector, c(mu, sigma, lambda)
#' @param d  a dataframe with a column `r` that indicate response (0,1) and a column `x` that indicate the stimulus, plus a column `condition`
#' @export

lnorm_3par <- function(p,d){
	-sum(log(psy_3par(d$x[d$r==1], p[1] ,p[2] ,p[3]))) - 
	 sum(log(1 - psy_3par(d$x[d$r==0], p[1] ,p[2], p[3])))
}