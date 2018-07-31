#' Signal processing
#'
#' gap fill-in  (cubic-spline/linear interpolation)
#' I used that to interpolate blink in pupil size recordings (e.g., Lisi Bonato & Zorzi, Biologial Psychology, 2015)
#' @param x signal
#' @param max maximum gape length (to be interpolated)
#' @param sp sampling period
#' @export

fillGap<-function(x,sp=1/60,max=50,type="linear"){
  ID<-1:length(x)
  blinkIndex<-which(is.na(x))
  okIndex<-which(!is.na(x))
  x_interp<-x
  diffBI<-c(0,diff(blinkIndex))
  gap<-0
  for(i in 1:length(blinkIndex)){
  	if(diffBI[i]>1){
  		if(gap==0){ # gap begin
  			gap<-1
  			beg<-i
  		}else{
  			gap<-0
  			end<-i-1
  			size<-length(beg:end)
  			if(size*(sp*1000) > max){
  				if(type=="cubic"){
  					okIndex<-c(okIndex,blinkIndex[beg:end])	
  					blinkIndex[beg:end]<-100*length(blinkIndex)
  				}
  			}
  			if(size*(sp*1000)<max & type=="linear" & blinkIndex[beg]>1){
  				lvs<-x_interp[blinkIndex[beg]-1] 	# last valid before gap
  				fvs<-x_interp[blinkIndex[end]+1] 	# first valid after gap
  				m <-(fvs-lvs)/size					# linear slope
  				x_interp[blinkIndex[beg:end]]<-m*(1:size)+lvs
	  		}
  		}
  	}
  }
  if(type=="cubic"){
	 blinkIndex<-blinkIndex[-which(blinkIndex>2*length(blinkIndex))]
	 x_interp[blinkIndex]<-spline(ID[okIndex],x[okIndex],xout=blinkIndex)$y
	 invisible(x_interp)
  }else if(type=="linear"){
  	invisible(x_interp)
  }else{
	warning("Error: unknown gap interpolation 'type' parameter!")
  }
}