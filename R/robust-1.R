#' Robust loss functions
#'
#' Tukey's bisquare loss function. (quadratic at first, then tapers off).
#' Note that you need to have an estimate of the residual's standard deviation (call it sigma_hat)
#' to set the parameter k, which determines where the loss function is quadratic and where it is not
#' typical values for k, which provide high efficiency also in the normal case, are then 
#' k = sigma_hat * 4.685 
#' @export

bisquare_loss <- function(err, k){
	# this start quadratic then flattens to a constant value
	ifelse(abs(err)<=k, ((k^2)/6) * (1 - (1-(err/k)^2)^3),  (k^2)/6)
} 