#' Signal processing
#'
#' 3-samples window velocity (smooth first derivative)
#' @param x signal
#' @param t timestamp (ms)
#' @export

vel3 <- function(x,t){
	N  <- length(x)
	v  <- rep(0,N)
	v[2:(N-1)]<- colMeans(rbind((x[2:(N-1)]-x[1:(N-2)]),(x[3:(N)]-x[2:(N-1)])),na.rm=T) / colMeans(rbind((t[2:(N-1)]-t[1:(N-2)]),(t[3:(N)]-t[2:(N-1)])),na.rm=T)
}
