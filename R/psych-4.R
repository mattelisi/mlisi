#' Psychometric
#'
#' predicted choice probability (multiple conditions, constant lapse rate)
#' @param p parameter vector, c(mu, sigma, lambda)
#' @param d  a dataframe with a column `r` that indicate response (0,1) and a column `x` that indicate the stimulus, plus a column `condition`
#' @export

predict_3par_multi <- function(p,d){
	np <- 1 + length(levels(d$condition))*2
	if(length(p) != np){stop("Length of parameter vector != 1 + length(unique(d$condition))*2")}
	pp <- rep(NA,nrow(d))
	for(i in 1:length(levels(d$condition))){
		pp[d$condition==levels(d$condition)[i]] <- psy_3par(d$x[d$condition==levels(d$condition)[i]], p[2*i-1], p[2*i], p[np])
	}
	return(pp)
}