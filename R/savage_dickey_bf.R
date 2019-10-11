#' Savage-Dickey density ratio
#'
#' Approximate the Savage-Dickey density ratio, and compute Bayes factor in favour of null hypothesis.
#' As input it takes a vector of samples, which should represent the posterior distribution of the quantity of interests, usually obtained through MCMC sampling.
#' The function assumes a Gaussian prior (the default is mean 0 and sd 1).
#' @param x vector of samples from posterior distribution
#' @param x_0 location of point-null hypothesis (usally zero)
#' @param prior.mean mean of Gaussian prior used to estiamte the model
#' @param prior.sd standard deviation of Gaussian prior used to estiamte the model
#' @param plot logical: should the prior and posterior probability be plotted?
#' @return Bayes factor, indicating how likely are the data under the null hypothesis, compared to the alternative.
#' @export

savage.dickey.bf<-function(x, x_0=0, prior.mean=0, prior.sd=1,plot=F){
  require(polspline)
  fit.posterior <- logspline(x)
  posterior_w <- dlogspline(x_0, fit.posterior)
  if(plot){
    R <- (fit.posterior$range[2] - fit.posterior$range[1])/3
    plot(fit.posterior,xlab="parameter value",ylab="density",lwd=2,xlim=c(fit.posterior$range[1]-R,fit.posterior$range[2]+R))#,log="y")
    x <- seq(fit.posterior$range[1]-R, fit.posterior$range[2]+R, length.out=500)
    lines(x, dnorm(x, mean=prior.mean,sd=prior.sd),col="red",lwd=2)
    abline(v=x_0,lty=2)
    points(x_0, posterior_w,pch=19,col="black")
    points(x_0, dnorm(x_0, prior.mean,prior.sd),pch=19,col="red")
    legend("topright",c("posterior","prior"),lwd=2,col=c("black","red"),pch=19,bty="n",inset=0.02)
  }
  
  cat(paste0("Approximate BF (Savage-Dickey) in favor of null x=",x_0," : ", round(posterior_w/dnorm(x_0, prior.mean,prior.sd),digits=2),"\n"))
  invisible(posterior_w/dnorm(x_0, prior.mean,prior.sd))
}
