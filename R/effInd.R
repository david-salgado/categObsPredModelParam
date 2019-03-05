#' @title Return a \linkS4class{list} with the global efficiency indicator and modelfits
#' for prior probabilities.
#'
#' @description \code{computeEdEfficiency} computes model fits for prior probabilities 
#' and the global efficiency indicator for a specific value of the regressand and a 
#' specific formula.
#' 
#' @param formula an object of class \code{\link[stats]{formula}} (or one that
#' can be coerced to that class): a symbolic description of the model to be
#' fitted. The details of model specification are given under ‘Details’.
#'
#' @param data data frame, list or environment (or object coercible by
#' \code{as.data.table} to a \linkS4class{data.table}) containing the variables
#' in the model (see 'Details').
#' 
#' @param targetValue value of the regressand (an object of class character).
#' 
#' @param id.vars names of the identification variables for every unit in the
#' input data set \code{data}.
#' 
#' @param designWeight an object of class character containing the design weight.
#' 
#' @param edEffIndicator efficiency indicator to evaluate the ordering given by the model
#' 
#' @param globalIndicator 
#' 
#' @param priorBin number of units in which the data is divided to construct the mesh of points
#' to calculate the efficiency indicator.
#' 
#' @param suffix parameter for the name of the edited version of the variable under analysis.
#' 
#' @param ...	extra arguments to be used to form the default control argument.
#' 
#' @return \linkS4class{list} with model fits for prior probabilities, description of 
#' the model to be fitted, value of the regressand and global efficiency indicator obtained 
#' for this specific value of the regressand.
#'
#' @examples
#'
#' \dontrun{
#' }
#'
#' @include Models.R regfit2.R computeProbs.R editPriority.R computeRunningEstim.R
#'
#' @import data.table StQ
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
