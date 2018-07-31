#' Signal processing
#'
#' ifinite-impulse-response (IIR) single-pole lowpass filter
#' @param x signal
#' @param fc cut-off frequency
#' @param sp sampling period
#' @export

filtLP<-function(x,fc,sp=1/60){
  RC<-1/(2*pi*fc)
  alpha<- sp/(sp+RC) # smoothing factor  
  for (i in 1:length(x)){
	if (!is.na(x[i])){
    if (i==1 || is.na(x[i-1])){
      x[i]<-x[i]
    }
    else{
      x[i]<-alpha*x[i] + (1-alpha)*x[i-1]
    }
	}	
  } 
  invisible(x)
}
 
