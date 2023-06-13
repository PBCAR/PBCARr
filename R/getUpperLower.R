#' Get Lower or Upper Triangles
#'
#' This is a function to obtain the lower or upper triangle of a matrix.
#'
#' @param cormat a matrix.
#' @param upper a logical.
#' @export get_tri

  get_tri<-function(cormat, upper=FALSE){
    if (upper) {
      cormat[upper.tri(cormat)] <- NA
    } else {
      cormat[lower.tri(cormat)]<- NA
    }
    return(cormat)
  }
