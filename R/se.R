#' Miscellaneous helpers
#'
#' Standard error of the mean (SEM)
#' @export

se <- function(v){
	sqrt(var(v,na.rm=T))/sqrt(length(v))
}