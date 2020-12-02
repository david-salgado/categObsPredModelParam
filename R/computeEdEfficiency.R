#' @title Return a \linkS4class{list} with the global efficiency indicator and modelfits
#' for prior probabilities.
#'
#' @description \code{computeEdEfficiency} computes model fits for prior probabilities 
#' and the global efficiency indicator for a specific value of the regressand and a 
#' specific formula.
#' 
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
#' @param varPriority variable with the priority of the unit.
#' 
#' @param designWeight an object of class character containing the design weight.
#' 
#' @param edEffIndicator efficiency indicator to evaluate the ordering given by the model
#' 
#' @param globalIndicator indicator for all the data at once.
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
#' }
#'
#' @include computeProbs.R computeRunningEstim.R
#'
#' @import data.table StQ
#'
#' @export
#' 

computeEdEfficiency <- function(data, targetValue, edEffIndicator, id.vars, varPriority, 
                                designWeight, globalIndicator, priorBin = 50, suffix){
  
  
  edPriority <- RB <- estim <- estim0 <- ARB <- NULL
  
  data <- data[order(get(varPriority))]
  
  # Filtramos para calcular la eficiencia
  N <- dim(data)[1]
  runningPriority <- c(seq(from = 0, to= N, by = priorBin), N)
  
  # Calculamos los estimadores a intervalos de unidades depuradas según runningPriority
  
  levelsTargetVar <- levels(as.factor(unique(data[[targetValue]])))
  
  Estim_run <- computeRunningEstim(
    edPriority = runningPriority, 
    data = data, 
    levelsTargetVar = levelsTargetVar,
    id.vars = id.vars, 
    varPriority = varPriority,
    targetVar = targetValue, 
    designWeight = designWeight, 
    suffix = suffix)

  # Estim_final <- Estim_run[edPriority == N] 
  Estim_final <- unique(Estim_run[edPriority == N]) 
  setnames(Estim_final, 'estim', 'estim0')[, edPriority := NULL]
  Estim_run <- Estim_run[Estim_final, on = targetValue][
    , RB := (estim - estim0) / estim0][
    , ARB := abs(RB)]
  
  # Calculamos la eficiencia de la ordenación
  Regval <- sort(unique(Estim_run[[targetValue]]))
  eff <- sapply(seq_along(Regval), function(i){effInd(as.matrix(Estim_run[get(targetValue) == Regval[i], c('edPriority', 'ARB'), with = FALSE]))})
  names(eff) <- Regval
  
  out <- eff
                    
  return(out)
}
