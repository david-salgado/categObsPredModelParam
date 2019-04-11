#' @title Return a \linkS4class{data.table} with the priority that each unit 
#' has to be analyzed.
#'
#' @description \code{editPriority} computes the priority for each unit in the
#' input data set \code{data}. The corresponding model is applied to each unit,
#' depending on the value of the regressand. The empirical distribution is used 
#' for every single value of the regressand, in order to assign a correct 
#' priority to units.
#' 
#' @param models A \linkS4class{data.table} with four colums named with the LHS
#' variable of \code{formula}, fit.pi, fit.1 and fit.0. The first column contains 
#' every single value of this variable in the input data set \code{data}. 
#' The second, third and fourth columns contain a \code{glm} object with the fit
#' of the model for each variable value.
#' 
#' @param data data frame, list or environment (or object coercible by
#' \code{as.data.table} to a \linkS4class{data.table}) containing the variables
#' in the model (see 'Details').
#' 
#' @param formula an object of class \code{\link[stats]{formula}} (or one that
#' can be coerced to that class): a symbolic description of the model to be
#' fitted. The details of model specification are given under ‘Details’.
#'
#' @param targetVar name of the regressand (an object of class character).
#' 
#' @param  regressors an object of class character containing the names of regressors.
#' 
#' @param designWeight an object of class character containing the design weight.
#' 
#' @param id.vars names of the identification variables for each unit in the
#' input data set \code{data}.
#' 
#' @param suffix parameter for the name of the edited version of the variable under analysis.
#' 
#' @return A \linkS4class{data.table} containing the following variables:
#' id.vars, regressand, regressors, design weight, P01, moments, regressors and the priority
#' to be analyzed.
#' 
#' @examples
#'
#' \dontrun{
#' }
#'
#' @include 
#'
#' @import data.table
#'
#' @export
#' 

editPriority <- function(models, data, formula, targetVar, regressors, 
                         designWeight, id.vars, suffix = '_ed'){
  
  # Calculamos las probabilidades de error
  #data <- computeProbs(models, id.vars, formula, data, designWeight)
  
  NAprobs <- data[is.na(P01)]
  if (dim(NAprobs)[1] > 0) warning('[editPriority] Error probabilities P01 with NA values detected.
                                   They will be imputed to 1.')
  
  data[is.na(P01), P01 := 1]

  naDesWeight <- data[is.na(get(designWeight))]
  if (dim(naDesWeight)[1] > 0) warning('[editPriority] Design weights with NA values detected 
                                       thus producing NA moments. They will be ranked last.')

  ecdffun <- function(x){
    
    if (all(is.na(x))) return(NA_real_)
    output <- ecdf(x)(x)
    return(output)
    
  }
  
  data <- data[
    , moment := ecdffun(as.numeric(get(designWeight)) * P01), by = targetVar][
    , c(id.vars, targetVar, designWeight , 'P01', 'moment',regressors), with = FALSE]
  out <- data[order(-moment, -get(designWeight), -P01)][, priority := .I]
  return(out)  
}

