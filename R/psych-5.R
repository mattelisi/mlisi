#' Psychometric
#'
#' Simulate psy function (e.g. for bootstrapping)
#' @param p parameter vector, c(mu, sigma, lambda)
#' @param d  a dataframe with a column `r` that indicate response (0,1) and a column `x` that indicate the stimulus, plus a column `condition`
#' @export

simulate_3par_multi <- function(p,d){
	pp <- predict_3par_multi(p,d)
	nt <- nrow(d)
	nr <- vector(length = nt, mode = "numeric")
	for(i in 1:nt) nr[i] <- rbinom(1,1,pp[i])
	return(nr)
}