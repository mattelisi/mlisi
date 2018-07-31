#' Signal processing
#'
#' velocity measure: basic calculation (first derivative)
#' @param x signal
#' @param t timestamp (ms)
#' @export

simpvel <- function(x,t){
	t <- t/1000
	N  <- length(x)
	v  <- rep(0,N)
	v[1:(N-1)]<-(x[2:(N)]-x[1:(N-1)])/(t[2:(N)]-t[1:(N-1)])
}