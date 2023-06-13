#' Check number of missing values
#'
#' This is a function to count the number of missing values.
#'
#' @param vect A numeric vector of values.
#' @param nonM A logical indicating whether to report the non-missing or missing counts.
#' @export checkM
#' @examples
#' x <- c(1:100, NA)
#' y <- checkM(x)

checkM <- function(vect, nonM = FALSE){
	if (nonM) {
		sum(!is.na(vect))
		} else {
		sum(is.na(vect))
		}
}
