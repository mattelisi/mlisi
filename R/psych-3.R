#' Psychometric
#'
#' (negative) log-likelihood calculation for multiple condition
#' the lapse rate is assumed invariant across conditions
#' @param p parameter vector, c(mu, sigma, lambda)
#' @param d  a dataframe with a column `r` that indicate response (0,1) and a column `x` that indicate the stimulus, plus a column `condition`
#' @export
#' @examples
#' # generate some some fake data
#' set.seed(1)
#' x <- runif(900)*6-3
#' condition <- rep(1:3,300)
#' d <- data.frame(x , condition)
#' d$condition <- as.factor(d$condition)
#' d$r <- NA
#' for(i in 1:sum(d$condition=="1")) {d$r[d$condition=="1"][i] <- rbinom(1,1,psy_3par(d$x[d$condition=="1"][i],0,0.5,0.05))}
#' for(i in 1:sum(d$condition=="2")) {d$r[d$condition=="2"][i] <- rbinom(1,1,psy_3par(d$x[d$condition=="2"][i],0,1,0.05))}
#' for(i in 1:sum(d$condition=="3")) {d$r[d$condition=="3"][i] <- rbinom(1,1,psy_3par(d$x[d$condition=="3"][i],0,1.5,0.05))}
#' 
#' # set reasonable intial guesses and boundaries, and do contrained optimization
#' start_p <- c(rep(c(0,1),3), 0.01)
#' l_b <- c(rep(c(-6,0.0001),3), 0)
#' u_b <- c(rep(c(6,6),3), 0.2)
#' fit <- optim(par = start_p, lnorm_3par_multi , d=d, method="L-BFGS-B",hessian = T, lower =l_b, upper =u_b)
#' 
#' # estimated parameters
#' round(fit$par,digits=5)
#' 
#' # standard error of the estimates 
#' sqrt(diag(solve(fit$hessian)))
#' 
#' # simple ggplot figure 
#' nd <- expand.grid(x=seq(-3,3,length.out=200),condition=unique(d$condition))
#' nd$r <- predict_3par_multi(fit$par, nd)
#' ggplot2::ggplot(d, aes(x=x,y=r))+geom_point(pch=19,col="blue",alpha=0.05)+geom_line(data=nd,color="black",lwd=0.5)+facet_grid(.~condition)+nice_theme


lnorm_3par_multi <- function(p,d){
	np <- 1 + length(levels(d$condition))*2
	if(length(p) != np){stop("Length of parameter vector != 1 + length(unique(d$condition))*2")}
	L <- 0
	for(i in 1:length(levels(d$condition))){
		L <- L + lnorm_3par(c(p[2*i-1], p[2*i], p[np]), d[d$condition==levels(d$condition)[i],])
	}
	return(L)
}
