#' Calculate a D score based on vectors of means, SDs, and Ns
#'
#' @param m1 mean of block 1
#' @param m2 mean of block 2
#' @param sd1 standard deviation of block 1
#' @param sd2 standard deviation of block 2
#' @param n1 n (count) of block 1
#' @param n2 n (count) of block 2
#'
#' @return vector of D scores
#'
#' @importFrom dplyr '%>%'
#'
#' @export

dScore <- function(m1, m2, sd1, sd2, n1, n2){
  # with a set of block mean latencies, sds, and ns, returns a d score
  numerator <- m1-m2
  denominator <- sqrt( ( ( (n1-1)*sd1^2+(n2-1)*sd2^2) +
                           ( (n1+n2) * ((m1-m2)^2 ) / 4) ) / (n1+n2-1))
  d <- numerator/denominator
  return(d)
}
