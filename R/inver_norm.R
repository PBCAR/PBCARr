#' Transform variables to normal
#'
#' This is a function to apply rank-based transformation to a variable  to have a normal distribution with
#' mean 0 and variance 1. Missing values in  are acceptable.
#'
#' @param x A numeric vector of values.
#' @param keep_mean_sd a logical indicating whether mean and sd should be kept.
#' @export inver_norm
#' @import stats
#' @examples
#' x <- stats::rchisq( n = 100, df=4)
#' y <- inver_norm(x)

inver_norm <- function(x, keep_mean_sd = TRUE){
	mm <- mean(x, na.rm=T)
	ss <- sd(x, na.rm=T)
    x <- stats::qnorm((rank(x, na.last="keep")-0.5)/sum(!is.na(x)))
    if (!keep_mean_sd) {
    x <- (x-mm)/ss
    } else {
    x <- (x+ mm)*ss
    }
    x
    }
