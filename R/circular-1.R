#' Circular data - helpers
#'
#' Signed angular difference
#' @export

angDiff <- function(alpha,beta){ 
	diff <- atan2(sin(beta-alpha), cos(beta-alpha))
	invisible(diff)
}
