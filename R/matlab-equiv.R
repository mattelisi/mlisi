#' Matlab equivalents
#'
#' E equivalents of Matlab `pol2cart`
#' @export

pol2cart <- function(th,r){
	x <- r*cos(th)
	y <- r*sin(th)
	invisible(cbind(x,y))
}
