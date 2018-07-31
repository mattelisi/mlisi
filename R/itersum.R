#' Miscellaneous helpers
#'
#' Iterative sum of each element of a vector with its first element
#' @export

itersum <- function(v){
	l <- length(v)
	v_out <- c(v[1], rep(NA,l-1))
	for(i in 2:l){
		v_out[i] <- v[1] + v[i]
	}
	return(unname(v_out))
}