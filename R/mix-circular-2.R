#' Circular-mixture
#'
#' likelihood function for mixture model
#' @param par vector: p(guess), mean direction, k
#' @export

p_mix <- function (par, d) 
{
		p <- par[1]/(2*pi) + (1-par[1]) * dvonMises(d,par[2],par[3])
		return(p)
}
