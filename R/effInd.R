#' @title Return the global efficiency indicator.
#'
#' @description \code{effInd} computes the global efficiency indicator. It is defined as the 
#' difference between the area under the polygonal line of the absolute relative pseudo-bias,
#' and the area under the straight line corresponding to random selection.
#' 
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
#'  fitPar <- new(Class = 'fitParam',
#'   edData = FFall_AS.StQ, rawData = FGall_AS.StQ, 
#'   selParam = list(ntreeTry=1000, stepFactor=2, improve=0.05, 
#'                   trace=TRUE, plot=TRUE, doBest = TRUE, 
#'                   ptrain = 0.8, DD = DDactu),
#'                   valParam = list(edEffInd = effInd, priorBin = 5, 
#'                   dataVal = c('Train','Test')))
#'                   
#'  ObsPredPar1 <- new(Class = 'categObsPredModelParam',
#'                   Data = FGall_AS.StQ,
#'                   VarRoles = list(Units = IDUnits,
#'                   Domains = character(0),
#'                   DesignW = DesignW,
#'                   Regressands = Regressands,
#'                   Regressors = Regressors
#'                   ))
#'                   
#' ObsPredPar1 <-  fitModels(ObsPredPar1, fitPar, na.as.category)
#' ObsPredPar1 <- computeVal(ObsPredPar1, fitPar, na.as.category)
#' # computeVal calls computeEdEfficiency calls (computeRunningEstim and effInd)
#' }#'
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
