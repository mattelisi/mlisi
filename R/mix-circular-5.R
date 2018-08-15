#' Circular-mixture
#'
#' wrapper function to fit the model and output the parameters as list
#' @param par vector: p(guess), mean direction, k
#' @param d vector: vector of direction angles
#' @export

fit_mixture <- function(v, se = T, method = "noboot", nsim = 1000) {

	start_par <- c(0.01, mean(v), (1/sd(v))/10)
	loB <- c(0, -pi, 0)
	upB <- c(1, pi, 10)
	if (!se) {
		ftm <- optimx::optimx(par = start_par, negLogLik_mix, d = v, method = "bobyqa", lower = loB, upper = upB)
		outV <- unlist(matrix(ftm[1, 1:3], 1, 3))
		names(outV) <- c("pu", "mu", "k")

	} else {
		ftm <- optimx::optimx(par = start_par, negLogLik_mix, d = v, method = "bobyqa", lower = loB, upper = upB)
		outV <- unlist(matrix(ftm[1, 1:3], 1, 3))
		outV <- c(outV, sqrt(diag(MASS::ginv(numDeriv::hessian(negLogLik_mix, x = outV, d = v)))))

		if (method == "boot") {

			bootmuse <- function(v, i) {
				start_par <- c(0.01, mean(v), (1/sd(v))/10)
				ftm <- optimx::optimx(par = start_par, negLogLik_mix, d = v, method = "bobyqa", lower = loB, upper = upB)
				outV <- unlist(matrix(ftm[1, 1:3], 1, 3))
				return(outV[2])
			}

			bootRes <- boot(v, bootmuse, nsim)
			outV[5] <- sd(bootRes$t, na.rm = T)
		}
		names(outV) <- c("pu", "mu", "k", "pu_se", "mu_se", "k_se")
	}
	return(outV)
}
