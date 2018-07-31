#' Display measures
#'
#' Convert pixels to degrees of visual angles
#' @param xRes horizontal screen resolution
#' @param screenWidth screen width in cm
#' @param viewDistance viewing distance in cm
#' @return degrees
#' @export

pix2deg <- function(xRes,screenWidth,viewDistance,PIXEL=1){
	phi <- atan2(1,viewDistance)*180/pi
	deg <- (phi/(xRes/screenWidth))*PIXEL
	return(deg)
}
