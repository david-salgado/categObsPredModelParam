#' @title Fit random forest model
#'
#' @description \code{RFfit} fits a random forest model for a specific value of the regressand and 
#' specific regressors. Return an object of class randomForest.
#'
#' @param data data frame, list or environment (or object coercible by
#' \code{as.data.table} to a \linkS4class{data.table}) containing the variables
#' in the model (see 'Details').
#' 
#' @param targetVarName value of the regressand (an object of class character).
#' 
#' @param parms names of the identification variables for every unit in the
#' input data set \code{data}.
#' 
#' @param regressors an object of class character containing the design weight.
#'  
#' @return an object of class randomForest with model fit.
#'
#' @examples
#'
#' \dontrun{
#'   fitPar <- new(Class = 'fitParam',
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
#' }
#'
#' @include 
#'
#' @import data.table StQ randomForest
#'
#' @export
#' 
#' 
RFfit <- function(data, targetVarName, parms, regressors){
  
  '..regressors' <- NULL
  
  ntreeTry <- parms$ntreeTry
  stepFactor <- parms$stepFactor
  improve <- parms$improve
  trace <- parms$trace
  plot <- parms$plot
  doBest <- parms$doBest
  
  target <- eval(parse(text=paste0('data$',targetVarName)))
  
  out <- randomForest::tuneRF(data[, ..regressors], target, 
                   ntreeTry=ntreeTry, stepFactor=stepFactor, improve=improve,
                   trace=trace, plot=plot, doBest = doBest)
  class(out)
  
  return(out)
  
}

