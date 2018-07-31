#' Psychometric
#'
#' predicted choice probability of a 3 parameter psychometric function with symmetric lapse rate
#' @export

psy_3par <- function(x, mu ,sigma, lambda){
	lambda + (1-2*lambda) * 0.5 * (1+erf((x-mu)/(sqrt(2)*sigma)))
}