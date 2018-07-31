#' General
#'
#' Adaptive outlier cut-off
#' @param rtv varable of interest (vector)
#' @param sv grouping factor
#' @param nsd distance from the mean at which is applied the cut-off (in standard deviations)
#' @export

outfilter<-function(rtv,sv,nsd=2)
{
	if(length(rtv)==length(sv)){
		index<-{}
		subjects<-unique(sv)
		for(i in 1:length(subjects)){
			aRT <- rtv[which(sv==subjects[i])]
			aM  <- mean(aRT)
			aSD <- sd(aRT)
			index <- c(index, which(aRT>(aM+nsd*aSD)|aRT<(aM-nsd*aSD)))
		}
		prop <- length(index)/length(rtv)
		cat("\n\t\tfilter at ",nsd,"sd will exclude ",100*round(prop,digits=3)," % of total obs.\n\n")
		invisible(index)
	}else{
		stop("Error: the two vectors have different length!")
	}
}