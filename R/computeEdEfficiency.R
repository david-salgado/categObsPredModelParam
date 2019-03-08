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

computeEdEfficiency <- function(formula, data, targetValue,id.vars, designWeight, edEffIndicator,
                                globalIndicator, maxit, priorBin = 50, suffix){
  
  
  # Especificamos parámetros de entrada
  formVariables <- all.vars(as.formula(formula))
  regressand <- formVariables[1]
  regressors <- setdiff(formVariables, regressand)
  # N <- dim(data)[1]
  # runningPriority <- c(seq(from = 0, to= N, by = priorBin), N)
  variables <- c(id.vars, regressand, paste0(regressand, suffix), regressors, designWeight,'GeoLoc_35._4._2.1.5._1.2.3.')
  data <- data[, ..variables]
  
  # Generamos los modelos de regresion logistica
  models<-Models(formula = formula, 
                 data = data, 
                 id.vars = id.vars,
                 targetValue = targetValue,
                 maxit = maxit, 
                 suffix = suffix)
  
  # Separamos en dos conjuntos de datos: dataFD y dataFF y generamos objeto StQ
  # 1. Datos brutos
  dataFD <- copy(data)[,paste0(regressand,suffix) := NULL][,Period := NULL]
  dataFD <- dataFD[get(regressand)==targetValue]
  dataFD_StQ <- melt_StQ(dataFD,poolDD0)
  dataFD_StQ <- StQListToStQ(BuildStQList(list(AA2011 = dataFD_StQ)))
  dataFD <- dcast_StQ(dataFD_StQ)
  
  # 2. Datos depurados
  dataFF <- copy(data)[,c(id.vars, paste0(regressand,suffix)),with=FALSE]
  #colnames(dataFF)[colnames(dataFF)==paste0(regressand,suffix)] <- regressand
  #dataFF_StQ <- melt_StQ(dataFF,poolDD0) 
  #dataFF_StQ <- StQListToStQ(BuildStQList(list(AA2011 = dataFF_StQ)))
  #dataFF <- dcast_StQ(dataFF_StQ)
  #colnames(dataFF)[colnames(dataFF)==regressand] <- paste0(regressand,suffix)
  
  # Generamos objeto de la clase categObsPredModelParam, donde se van a almacenar las probabilidades
  ObsDataFD <- new(Class = 'categObsPredModelParam',
                   Data = dataFD_StQ,
                   VarRoles = list(Units = id.vars,
                                   Domains = character(0),
                                   DesignW = designWeight,
                                   Regressands = regressand,
                                   Regressors = regressors))
  
  # Incluimos los modelos en el objeto ObsDataFF
  setModelFits(ObsDataFD) <- models
  
  # C?lculo de probabilidades
  ObsDataFD <- computeProbs(ObsDataFD)
  DataFD_prior <- ObsDataFD@probs
  
  # Filtramos para calcular la eficiencia
  N <- dim(DataFD_prior)[1]
  runningPriority <- c(seq(from = 0, to= N, by = priorBin), N)
  
  # Calculamos los momentos de error y establecemos la ordenaci?n de unidades
  DataFD_prior <- merge(DataFD_prior, dataFD[,c(id.vars, designWeight),with=FALSE], by = id.vars, all.x = TRUE)
  DataFD_prior <- editPriority(models = models, 
                               data = DataFD_prior,
                               formula = formula, 
                               targetVar = regressand,
                               regressors = regressors, 
                               designWeight = designWeight, 
                               id.vars = id.vars)
  
  DataFD_prior <- merge(DataFD_prior, dataFF[, c(id.vars, paste0(regressand, suffix)),with=FALSE], by = id.vars, all.x = FALSE)
  #DataFD_prior <- DataFD_prior[dataFF[, c(id.vars, paste0(regressand, suffix)), with = FALSE], on = id.vars]
  DataFD_prior <- DataFD_prior[order(priority),]
  
  # Cálculo de momentos
  #ImpParam <- new(Class = 'MeanImputationParam',
  #                 VarNames = regressand,
  #                DomainNames =  c('GeoLoc_35._4._2.1.5._1.2.3.'))
  
  #AbsLossPar <- new(Class = 'AbsLossErrorMomentParam',
  #                  VarNames =  regressand,
  #                  Homoskedastic = c(TRUE),
  #                  Imputation = ImpParam)
  
  #ErrorMoments <- ComputeErrorMoment(ObsDataFF, AbsLossPar)
  
  
  
  #units_lot1 <- ErrorMoments@Units
  #vars <- c(id.vars, designWeight)
  #UnitPriorParam <- new(Class = 'UnitPrioritizationParam',
  #                      UnitScFunction = 'MinkUnitSc',
  #                      ScFunctionParam =  list(alpha = 1, Weights = 1),
  #                      DesignW = list(regressand = as.numeric(dcast_StQ(dataFF_StQ)[units_lot1, on = id.vars][["Parametro_07._6.1._2.1.5.1."]])))
  #
  #UnitPrioritization <- PrioritizeUnits(ErrorMoments, UnitPriorParam)
  
  # Calculamos los estimadores a intervalos de unidades depuradas según runningPriority
  
  levelsTargetVar <- levels(as.factor(unique(dataFD[[regressand]])))
  
  Estim_run <- computeRunningEstim(
    edPriority = runningPriority, 
    data = DataFD_prior, 
    levelsTargetVar = levelsTargetVar,
    id.vars = id.vars, 
    targetVar = regressand, 
    designWeight = designWeight, 
    suffix = suffix)

  # Estim_final <- Estim_run[edPriority == N] 
  # Jara Poch creo que es necesario para los casos en los que N es multiplo de priorbin
  Estim_final <- unique(Estim_run[edPriority == N]) 
  setnames(Estim_final, 'estim', 'estim0')[, edPriority := NULL]
  Estim_run <- Estim_run[Estim_final, on = regressand][
    , RB := (estim - estim0) / estim0][
    , ARB := abs(RB)]
  
  # Calculamos la eficiencia de la ordenación
  Regval <- sort(unique(Estim_run[[regressand]]))
  eff <- sapply(seq_along(Regval), function(i){edEffIndicator(as.matrix(Estim_run[get(regressand) == Regval[i], c('edPriority', 'ARB'), with = FALSE]))})
  names(eff) <- Regval
  modInd <- globalIndicator(eff)
  names(modInd) <- formula
  finalOutput <- list(models = models,
                      formula = formula,
                      randSelIndicators = eff,
                      global = modInd
                      )
  return(finalOutput)
}
