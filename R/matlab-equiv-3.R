#' Matlab equivalents
#'
#' R equivalent of Matlab `repmat`
#' @export

repmat <- function(X,m,n){
	mx = dim(X)[1]
	nx = dim(X)[2]
	matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}
