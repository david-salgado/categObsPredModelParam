#' @title Return \linkS4class{data.table} with model fits for prior probabilities
#'
#' @description \code{Models} computes ...
#'
#' @param formula 
#'
#' @param data 
#' 
#' @param id.vars
#' 
#' @param targetValue
#' 
#' @param maxit
#' 
#' @param suffix
#' 
#' @return \linkS4class{data.table} with ...
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

models <- models_pi[models_cond1, on = targetVars[1]][models_cond0, on = targetVars[1]]

return(models)
}