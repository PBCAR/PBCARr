#' Transform variables to normal
#'
#' This is a function to apply rank-based transformation to a variable  to have a normal distribution with
#' mean 0 and variance 1. Missing values in  are acceptable.
#'
#' @param x A numeric vector of values.
#' @export
#' @examples
#' x <- stats::rchisq( n = 100, df=4)
#' y <- inver_norm(x)

inver_norm <- function(x){
x <- stats::qnorm((rank(x, na.last="keep")-0.5)/sum(!is.na(x)))
x <- (x-mean(x, na.rm=T)/sd(x, na.rm=T))
x
}