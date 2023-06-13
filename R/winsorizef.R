#' Winsorize variables to eliminate outliers
#'
#' This is a function to apply winsorize a variable to
#' remove large values outside of the 3.5 standard deviations (usually for normal variables).
#'
#' @param x A numeric vector of values.
#' @export winsorizef
#' @examples
#' x <- stats::rchisq( n = 100, df=4)
#' y <- winsorizef(x)

winsorizef <- function(x) {
	 ifelse(abs(x- mean(x, na.rm=T)) <  sd(x, na.rm=T)*3.5, x, NA)
}
