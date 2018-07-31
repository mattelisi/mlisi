#' Circular-mixture
#'
#' Density of Von Mises distributionlapse rate
#' @export

dvonMises <- function (x,mu=0,k=10){ 
	(1/(2*pi*besselI(k, nu=0)))*exp(k*cos(x-mu))
}