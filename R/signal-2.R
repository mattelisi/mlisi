#' Signal processing
#'
#' central non-weighted moving-average filter
#' @param x signal
#' @param n window size
#' @export

filtCMA<-function(x, n){
  if(is.even(n)){
	  y<-{}
	  if (!is.null(x)){
		  y[1:n]<-x[1:n]
		  for (i in 1:length(x)){
			if (!is.na(x[i])){
		  		if (i>n){
					y[i]<-mean(c(x[(i-n):(i-1)], 
					x[i],
					x[(i+1):(i+n)]),na.rm=T)
		    	}
			}	
		  }
		  if (length(x)==length(y)){
		  	invisible(y) 
			}
		  else{
			  y<-c(y,rep(NA,length(x)-length(y)))
		  }
	  }
  }else{
  	warning("Error: filter window size must be an even number!")
  }
}
