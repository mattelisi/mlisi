#' Circular data - helpers
#'
#' Angular mean
#' @export

angMean <- function(v){ 
	atan2(sum(sin(v))/length(v), sum(cos(v))/length(v))
}
