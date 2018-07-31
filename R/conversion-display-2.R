#' Display measures
#'
#' Convert pixels to degrees of visual angles
#' @param xRes horizontal screen resolution
#' @param screenWidth screen width in cm
#' @param viewDistance viewing distance in cm
#' @param DEGS how many degree to convert?
#' @return pixels
#' @export

deg2pix <- function(xRes,screenWidth,viewDistance,DEGS=1){
	phi <- atan2(1,viewDistance)*180/pi
	pix <- DEGS/(phi/(xRes/screenWidth))
	return(pix)
}
