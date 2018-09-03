#' Miscellaneous helpers
#'
#' Bootstrapped 95% CI (BCA / percentile method)
#' @param nsim number of bootstrap simulations (default is 1000)
#' @param method Either "bca" (default, accellerated & bias-corrected, Efron, 1987), or "percentile"
#' @param alpha Either "bca" (accellerated & bias-corrected, Efron, 1987), or "percentile"
#' @return the 95% confidence limits
#' @export

bootMeanCI <- function(v,nsim=1000,method="bca", alpha=0.05, ...){
	bootmean <- function(v,i) mean(v[i],na.rm=T,...)
	bootRes <- boot::boot(v,bootmean,nsim)
	
	if(method=="percentile"){
	  
	  return(c(quantile(bootRes$t, probs = alpha/2,na.rm=T),quantile(bootRes$t, probs = 1-alpha/2,na.rm=T)))
	
	  }else if(method=="bca"){
	  
	    # estimate bias in std. norm deviates
	    obs <- mean(v)
	    b=qnorm((sum(bootRes$t > obs)+sum(bootRes$t==obs)/2)/nsim)
	    
	    # estimate acceleration constant
	    n=length(v) ; n1=n-1 ; obsn=obs*n
	    pv=i=0 ; while(i < n){i=i+1 ; pv[i]=obsn-n1*mean(v[-i])}
	    je=mean(pv)-pv
	    a=sum(je^3)/(6*sum(je^2))^(3/2)
	    
	    z=qnorm(c(alpha/2,1-alpha/2)) # Std. norm. limits
	    p=pnorm((z-b)/(1-a*(z-b))-b) # correct & convert to proportions
	    
	    return(quantile(bootRes$t,p=p)) # ABC percentile lims.
	}
}

