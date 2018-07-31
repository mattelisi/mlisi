#' Miscellaneous helpers
#'
#' Error function (approximation taken from: numerical recipes in C, 2nd ed)
#' @export

erf <- function(x)
{
	t <- 1/(1+0.5*abs(x))
	tau <- t*exp(-x^2 -1.26551223 + 1.00002368*t + 0.37409196*t^2 + 0.09678418*t^3 - 0.18628806*t^4 + 0.27886807*t^5 - 1.13520398*t^6 + 1.48851587*t^7 - 0.82215223*t^8 + 0.17087277*t^9)
	return(ifelse(x>=0, 1-tau, tau -1))
}
