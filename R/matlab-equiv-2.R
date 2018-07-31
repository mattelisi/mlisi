#' Matlab equivalents
#'
#' R equivalent of Matlab `cart2pol` 
#' @export

cart2pol <- function(x,y){
	th <- atan2(y,x)
	r <- sqrt(abs(x^2) + abs(y^2))
	invisible(cbind(th,r))
}

