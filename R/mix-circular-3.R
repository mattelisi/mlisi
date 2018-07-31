#' Circular-mixture
#'
#' compute the negative log-likelihood for the mixture model
#' @param par vector: p(guess), mean direction, k
#' @param d vector: vector of direction angles
#' @export

negLogLik_mix <- function (par, d) {
	return(-sum(log(p_mix(par, d))))
}
