#' @title Return \linkS4class{data.table} with model fits for prior probabilities.
#'
#' @description \code{Models} computes model fits for prior probabilities for each value or 
#' a specific value of the regressand and a specific formula.
#'
#' @param formula an object of class \code{\link[stats]{formula}} (or one that
#' can be coerced to that class): a symbolic description of the model to be
#' fitted. The details of model specification are given under ‘Details’.
#'
#' @param data data frame, list or environment (or object coercible by
#' \code{as.data.table} to a \linkS4class{data.table}) containing the variables
#' in the model (see 'Details').
#' 
#' @param id.vars names of the identification variables for each unit in the
#' input data set \code{data}.
#' 
#' @param targetValue value of the regressand (an object of class character).
#' 
#' @param maxit
#' 
#' @param suffix parameter for the name of the edited version of the variable under analysis.
#' 
#' @return A \linkS4class{data.table} with four colums named with the LHS
#' variable of \code{formula}, fit.pi, fit.1 and fit.0. The first column contains 
#' every single value of this variable in the input data set \code{data}. 
#' The second, third and fourth columns contain a \code{glm} object with the fit
#' of the model for each variable value.
#'
#' @examples
#'
#' \dontrun{
#' }
#'
#' @include regfit2.R
#'
#' @import data.table
#'
#' @export
#' 
Models <- function(formula, data, id.vars, targetValue = '', maxit, suffix = '_ed'){

  targetVars <- all.vars(as.formula(formula))
  
  cat('Fitting models for marginal probabilities...')
  models_pi <- regfit2(formula = formula, data = data,
                       id.vars= id.vars, targetValue = targetValue,
                       maxit = maxit, suffix = suffix, cond = '')
  setnames(models_pi, 'fit' ,'fit.pi')
  cat(' ok.\n')
  
  cat('Fitting models for probabilities conditional upon value 1...')
  models_cond1 <- regfit2(formula = formula, data = data,
                          id.vars= id.vars, targetValue = targetValue,
                          maxit = maxit, suffix = suffix, cond = 1)
  setnames(models_cond1, 'fit', 'fit.1')
  cat(' ok.\n')

  cat('Fitting models for probabilities conditional upon value 0...')
  models_cond0 <- regfit2(formula = formula, data = data,
                          id.vars= id.vars, targetValue = targetValue,
                          maxit = maxit, suffix = suffix, cond = 0)
  setnames(models_cond0, 'fit', 'fit.0')
  cat(' ok.\n')

  models <- models_pi[
    models_cond1, on = targetVars[1]][
    models_cond0, on = targetVars[1]]

  return(models)
}