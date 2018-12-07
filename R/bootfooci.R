#' Miscellaneous helpers
#'
#' Bootstrapped 95% CI for arbitratry function foo (BCA / percentile method)
#' @param foo is the function that compute the statistic of interest
#' @param nsim number of bootstrap simulations (default is 1000)
#' @param method Either "bca" (default, accellerated & bias-corrected, Efron, 1987), or "percentile"
#' @param alpha Either "bca" (accellerated & bias-corrected, Efron, 1987), or "percentile"
#' @return the 95% confidence limits
#' @export

bootFooCI <- function(v,nsim=1000,foo="mean", method="bca", alpha=0.05, ...){
  foo <- match.fun(foo)
	bootFoo <- function(v,i) foo(v[i],...)
	bootRes <- boot::boot(v,bootFoo,nsim)

	if(method=="percentile"){

	  return(c(quantile(bootRes$t, probs = alpha/2,na.rm=T),quantile(bootRes$t, probs = 1-alpha/2,na.rm=T)))

	  }else if(method=="bca"){

	    # estimate bias in std. norm deviates
			# The bias-correction parameter is related to the proportion of
			# bootstrap estimates that are less than the observed statistic
	    obs <- foo(v)
	    b <- qnorm((sum(bootRes$t > obs)+sum(bootRes$t==obs)/2)/nsim)

	    # estimate acceleration parameter
			# The acceleration parameter, a, is proportional to the skewness
			# of the bootstrap distribution
	    n <- length(v)
	    n1 <- n-1
	    obsn <- obs*n
	    pv <- i <- 0
	    while(i < n){i=i+1
	      pv[i] <- obsn-n1*mean(v[-i])
	    }
	    je <- mean(pv)-pv
	    a <- sum(je^3)/(6*sum(je^2))^(3/2)

	    z <- qnorm(c(alpha/2,1-alpha/2)) # Std. norm. limits
	    p <- pnorm((z-b)/(1-a*(z-b))-b) # correct & convert to proportions

	    return(quantile(bootRes$t,p=p)) # ABC percentile lims.
	}
}
