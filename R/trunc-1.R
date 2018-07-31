#' Truncated normal distribution
#'
#' Density function of truncated normali distribution. See \link[=https://en.wikipedia.org/wiki/Truncated_normal_distribution]{Wikipedia}
#' @param mu mean of `un-truncated` distribution
#' @param mu standard deviation of `un-truncated` distribution
#' @param a minimum value
#' @param b maximum value
#' @return probability density
#' @export

dtruncnorm <- function(x, mu=0, sigma=1, a=-Inf,b=Inf){
	epsilon <- (x - mu)/sigma
	alpha <- (a - mu)/sigma
	beta <- (b - mu)/sigma
	Z <- pnorm(beta) - pnorm(alpha)
	ifelse(x>=a & x<b, dnorm(epsilon)/(sigma*Z), 0)
}