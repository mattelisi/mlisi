#' Robust loss functions
#'
#' Huber loss function. (quadratic at first, then become linear).
#' Note that you need to have an estimate of the residual's standard deviation (call it sigma_hat)
#' to set the parameter k, which determines where the loss function is quadratic and where it is not
#' typical values for k, which provide high efficiency also in the normal case, are then 
#' k = sigma_hat * 1.345
#' @export


huber_loss <- function(err, k){
	# this start quadratic and become linear beyond k
	ifelse(abs(err)<=k, 0.5 * err^2,  k*abs(err)-0.5*k^2)
} 
