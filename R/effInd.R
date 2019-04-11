#' @title Return the global efficiency indicator.
#'
#' @description \code{effInd} computes the global efficiency indicator. It is defined as the 
#' difference between the area under the polygonal line of the absolute relative pseudo-bias,
#' and the area under the straight line corresponding to random selection.
#' 
#' @param formula an object of class \code{\link[stats]{formula}} (or one that
#' can be coerced to that class): a symbolic description of the model to be
#' fitted. The details of model specification are given under ‘Details’.
#'
#' @param matrix an object of class matrix, containing two columns: 
#' the absolute relative pseudo-bias, ARB, for every value in the column edPriority, which
#' represents the number of units analyzed.
#' 
#' @return an object of class numeric with the value of global efficiency indicator.  
#'
#' @examples
#'
#' \dontrun{
#' }
#'
#' @import 
#'
#' @export
#' 

effInd <- function(matrix){
  x <- matrix[, 1]
  y <- matrix[, 2]
  areaARB <- sum(diff(x) * 0.5 * (y[-1] + y[-length(y)]))
  areaRand <- 0.5 * (x[length(x)] - x[1]) * y[1]
  if (is.na(areaRand)) return(NA)
  if (abs(areaRand) < .Machine$double.xmin) return(1)
  output <- 1 - areaARB / areaRand
  return(output)
}
