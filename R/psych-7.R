#' Psychometric
#'
#' Bootstrap model to estimate parameter posterior distribution (useful for calculating confidence intervals)
#' @param p parameter vector, c(mu, sigma, lambda)
#' @param d  a dataframe with a column `r` that indicate response (0,1) and a column `x` that indicate the stimulus
#' @export

boot_3par_multi <- function(p, d, l_b, u_b, nsim=1000){
	boot_es <- matrix(NA,ncol=length(p),nrow=nsim)
	np <- length(p)
	for(bi in 1:nsim){
		d$r <- simulate_3par_multi(p,d)
		#boot_es[bi,] <- optim(par = p, lnorm_3par_multi , d=d, hessian = F,method="L-BFGS-B", lower =l_b, upper =u_b)$par
		ftm <- optimx::optimx(par = p, lnorm_3par_multi , d=d,  method="bobyqa", lower =l_b, upper =u_b)
		boot_es[bi,] <- unlist(matrix(ftm [1,1:np],1,np))
	}
	return(boot_es)
}