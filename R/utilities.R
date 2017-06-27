### Miscellaneous utility functions


#' Precedent - takes a vector and returns the value of the vector that returns the value that precedes each observation.  For example, if a vector of trial types is c("congruent", "congruent", "incongruent", "congruent"), the function will provide the vector of trial types that *preceded* those in the vector.  Essentially, this just shifts the vector forward by one.  When using this, be sure to discard the first values in your blocks.
#'
#' @param x a vector of observations
#'
#' @return a shifted vector indicating the values that preceded the input values
#' @export
#'
#' @examples
precedent <- function(x){
  length <- length(x)
  y <- character()
  y[1] <- NA
  y <- append(y, x[1:length - 1])
  return(y)
}
