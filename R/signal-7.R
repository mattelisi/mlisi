#' Signal processing
#'
#' Align different epochs based on event of interest (useful for Tobii Eyetrackers)
#' @param x signal
#' @param evento factor with lenght equal to x (signal) indicating for each sample the trial event ("fixation", "target", etc.
#' @param flag selected event to align with (epochs will be time locked to the beginning of this event)
#' @param tw length of temporla window in sec
#' @param flag cut-off frequency
#' @param sp sampling period (secs, e.g. 1/60)
#' @param maxEpochNa max proportion of missing values (NA) in retained epochs (0:1)
#' @export

splitGazeData <- function(x, evento, flag, tw, sp = 1/60, n_before = 0.5, maxEpochNa = 0.1) {
	n_after <- floor((tw - n_before)/sp)
	n_before <- n_before/sp
	y <- {}
	for (i in 1:length(x)) {
		if ((i > n_before) && (evento[i] == flag) && (evento[i - 1] != flag)) {
			from <- i - n_before
			to <- i + n_after
			epoch <- x[from:to]
			if (sum(is.na(epoch))/length(epoch) <= maxEpochNa) {
				y <- rbind(y, epoch)
			}
		}
	}
	cat("     Counting epochs:", "\n")
	if (!is.null(y)) {
		cat("      - ", (dim(y)[1]), " epochs aligned", "\n")
	} else {
		cat("      -  0 epochs aligned...", "\n")
	}
	invisible(y)
}
