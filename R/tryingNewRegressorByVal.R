#' @title Returns a \linkS4class{list} with the global efficiency indicator and model fits for
#' prior probabilities for the best model selected.
#'
#' @description \code{tryingNewRegressorByVal} computes all possible model fits for 
#' \code{regformula}, taking into account a new additional regressor among those 
#' existing in \code{listRegressors}. 
#' Also, \code{tryingNewRegressorByVal} compares all fits and chooses the best model,
#' according to the global efficiency indicator. For the selected model, model fits for
#' prior probabilities is returned.
#' 
#' @param regformula an object of class \code{\link[stats]{formula}} (or one that
#' can be coerced to that class): a symbolic description of the model to be fitted.
#'
#' @param listRegressors an object of class character containing the name of all regressors
#' to possibly be considered in the model fit. 
#' 
#' @param data data frame, list or environment (or object coercible by
#' \code{as.data.table} to a \linkS4class{data.table}) containing the variables
#' in the model (see 'Details').
#' 
#' @param targetValue value of the regressand (an object of class character).
#' 
#' @param id.vars names of the identification variables for each unit in the
#' input data set \code{data}.
#' 
#' @param designWeight an object of class character containing the design weight.
#' 
#' @param parms input parameters required for the process:
#' "maxit" for model fits,
#' "edEffInd" is a function used to define the global efficiency indicator,
#' "priorBin" contains the number of units in which the data is divided to construct 
#' the mesh of points to calculate the efficiency indicator.
#' 
#' @param suffix parameter for the name of the edited version of the variable under analysis.
#' 
#' @return \linkS4class{list} containing three elements: 
#' The first one is a \linkS4class{list} with models fits for prior probabilities, description of 
#' the model to be fitted, value of the regressand and global efficiency indicator obtained 
#' for this specific value of the regressand.
#' The second one contains a symbolic description of the best model chosen among all models tried.
#' Last one element contains all models tried and the global efficiency indicator for each one. 
#' 
#' @examples
#'
#' \dontrun{
#' }
#'
#' @include computeEdEfficiency.R Models.R regfit2.R computeProbs.R editPriority.R 
#' computeRunningEstim.R
#'
#' @import data.table StQ
#'
#' @export
#' 

tryingNewRegressorByVal<- function(regformula, listRegressors, data, targetValue, id.vars,
                                   designWeight, parms, suffix){
  count<-0
  
  edEffInd <- parms$edEffInd
  priorBin <- parms$priorBin
  globalInd <- parms$globalInd
  maxit <- parms$maxit
  
  if (grepl('~', regformula)==TRUE) {
    formVariables <- all.vars(as.formula(regformula))
    listRegressors <- setdiff(listRegressors, formVariables)
  }
 
  for (var in listRegressors) {
    
    # #Para construir la formula de la regresion
    if (grepl('~', regformula)==TRUE) {
      formVariables <- all.vars(as.formula(regformula))
      regressand <- formVariables[1]
    } else {
      formVariables<-regformula
      regressand <- regformula
    }
    
    regressors <- c(setdiff(formVariables, regressand), var)
    formula<-paste(regressand, paste(regressors, collapse=" + "), sep=" ~ ")
    
    # print(paste0('Iteracion ',count, ': ', formula))
    comb1<-unique(data[get(regressand)==targetValue,..regressors])
    comb2<-unique(data[(get(regressand)==targetValue & get(regressand)==get(paste0(regressand,suffix))),..regressors])
    comb3<-unique(data[(get(regressand)==targetValue & get(regressand)!=get(paste0(regressand,suffix))),..regressors])
    
    #No tenemos en cuenta los modelos en los que las regresoras toman un solo valor
    if (unique(comb1[,.N])>1 & unique(comb2[,.N])>1 & unique(comb3[,.N])>1) 
    {
      # Para cada regresion almacenamos los indicadores de eficiencia:
      count<-count+1
      results<-computeEdEfficiency(formula = formula, 
                                   data = data,
                                   targetValue = targetValue,
                                   id.vars = id.vars, 
                                   designWeight =designWeight , 
                                   edEffIndicator= edEffInd, 
                                   globalIndicator = globalInd,
                                   maxit = maxit,
                                   priorBin = priorBin,
                                   suffix = suffix)
      print(paste0('Iteracion ',count, ': ', formula, ' effGlobal: ', results$global))
      
      # Almacenamos en una lista los indicadores de eficiencia de la regresion:
      if (count==1) {results_all<-results 
      allModels<-results$global
      # } else { results_all<-append(results_all, list(results))} #guardo todos los resultados
      } else {if(results$global>results_all$global){results_all<-results} #solo guardo el mejor
        allModels<-append(allModels,results$global) 
      }
    }
    
    
    #rm(results)
    rm(regressand)
    rm(formVariables)
    rm(regressors)
    #rm(formula)
    gc()  
  }
  
  # bestGlobal<-max(unlist(lapply(results_all, '[', c('global'))))
  # for (i in 1:length(results_all))  {  
  #   if (as.numeric(results_all[[i]]$global)==bestGlobal) 
  #     {
  #       formBestModel=results_all[[i]]$formula
  #     }
  # }
  if (count==0) { results_all<-list(formula=formula, global=0)
  allModels<-results_all$global
  }
  formBestModel<-results_all$formula
  bestGlobal<-results_all$global
  print(paste0('Best Model: ', formBestModel, ' globalIndicator: ', bestGlobal))
  print('--------------')
  out<-list(results_all, formBestModel,allModels)
  
  return(out)
}

