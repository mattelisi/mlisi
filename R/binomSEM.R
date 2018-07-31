#' Miscellaneous helpers
#'
#' Binomial standard error (input is a vector of binomila 0/1 outcomes)
#' @export

binomSEM <- function(v)
{
	sqrt((mean(v) * (1-mean(v)))/length(v))
}