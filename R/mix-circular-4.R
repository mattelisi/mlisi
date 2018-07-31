#' Circular-mixture
#'
#' transform concentration parameter k into a standard deviation
#' @export

k2SD <- function(k)
{
	sqrt(-2 *log(besselI(k, nu=1)/besselI(k, nu=0)))
}
