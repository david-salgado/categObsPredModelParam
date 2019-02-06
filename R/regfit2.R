#' @title Fit robust logistic regression models to values of an input variable
#'
#' @description This function is basically a wrapper for function
#' \code{\link{robustlogrm}} to fit a logistic regression model on each value
#' of an input variable of the data set.
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
#' @param cond parameter indicating the model to adjust according to:
#' \enumerate{
#'   \item \code{''} for the \eqn{\pi} model (marginal probability for the
#'   target variable value).
#'   \item \code{'0'} (or \code{0} or \code{FALSE}) for the model conditional on
#'   value \eqn{0} for the target binary variable value).
#'   \item \code{'1'} (or \code{1} or \code{TRUE}) for the model conditional on
#'   value \eqn{1} for the target binary variable value).
#' }
#'
#' @param suffix parameter for the name of the edited version of the variable
#' under analysis (see 'Details').
#'
#' @param ...	extra arguments to be used to form the default control argument.
#'
#' @details Regarding \code{formula} see \code{\link{robustlogrm}}. Although the
#' target variables for models with \code{cond=''}, on the one hand, and with
#' \code{cond=0,1}, on the other hand, are different (edited in the first case
#' and raw in the latter), it is not necessary to specify the suffix in the
#' formula.
#'
#' The input data set \code{data} must contain at least the following colums:
#' \enumerate{
#'
#' \item \code{id.vars}: columns with the identification variables of each unit.
#'
#' \item raw target variable: column with the raw values of the variable under
#' analysis. This is the LHS variable of \code{formula}.
#'
#' \item edited target variable: column with the edited values of the variable
#' under analysis. The name must be the same as that of the raw target variable
#' with a suffix specified in the parameter \code{suffix}.
#'
#' \item regressors: columns corresponding to the variables specified in the RHS
#' of \code{formula}.
#' }
#'
#'
#' @return A \linkS4class{data.table} with two colums named with the LHS
#' variable of \code{formula} and \code{fit}. The first column contains every
#' single value of this variable in the input data set \code{data}. The second
#' column contains a \code{glm} object with the fit of the model for each
#' variable value.
#'
#' @examples
#' \dontrun{
#' regfit2(formula= 'y ~ x1', data= trainDT, id.vars= 'ID', cond= '', suffix= '')
#' }
#'
#' @import data.table
#'
#' @include robustlogrm.R
#'
#' @export
regfit2 <- function(formula, data, id.vars, cond, suffix='_ed', ...){


  varNames <- all.vars(as.formula(formula))
  targetVarName <- varNames[1]
  targetVarName_ed <- paste0(as.character(targetVarName), suffix)
  regressors <- setdiff(varNames, targetVarName)
  data <- as.data.table(data)
  targetValues <- sort(unique(union(data[[targetVarName]], data[[targetVarName_ed]])))

  if (cond == ''){

    formula <- as.formula(gsub(targetVarName, 'bin', formula))
    auxData <- copy(data)[get(targetVarName_ed) == '',
                          (targetVarName_ed) := 'blank'][
                            , (targetValues) := lapply(targetValues, function(targetVal){targetVal == get(targetVarName_ed)})][
                              , (targetVarName_ed) := NULL]
    DT <- melt(auxData, id.vars = c(id.vars, regressors),
               measure.vars = targetValues, variable.name = targetVarName_ed,
               value.name = 'bin')
    out <- DT[, list(fit = list(robustlogrm(formula, data = .SD, ...))), by = targetVarName_ed]#[
    #      get(targetVarName_ed) == 'blank', (targetVarName_ed) := '']
    setnames(out, targetVarName_ed, targetVarName)

  }

  cond <- as.logical(as.integer(cond))
  if (cond %in% c(TRUE, FALSE)){

    formula <- as.formula(gsub(targetVarName, 'bin', formula))

    auxData <- copy(data)[get(targetVarName) == '', (targetVarName) := 'blank'][
      get(targetVarName_ed) == '', (targetVarName_ed) := 'blank'][
        , (targetValues) := lapply(targetValues, function(targetVal){targetVal == get(targetVarName)})]


    auxData_ed <- copy(data)[get(targetVarName_ed) == '', (targetVarName_ed) := 'blank'][
      get(targetVarName) == '', (targetVarName) := 'blank'][
        , (targetValues) := lapply(targetValues, function(targetVal){targetVal == get(targetVarName_ed)})]

    DT <- melt(auxData,
               id.vars = c(id.vars, regressors, targetVarName_ed, targetVarName),
               measure.vars = targetValues, variable.name = 'value_bin',
               value.name = 'bin')

    DT_ed <- melt(auxData_ed,
                  id.vars = c(id.vars, regressors, targetVarName_ed, targetVarName),
                  measure.vars = targetValues, variable.name = 'value_bin_ed',
                  value.name = 'bin_ed')

    DT[, value_bin_ed := DT_ed[['value_bin_ed']]][
      , bin_ed := DT_ed[['bin_ed']]]

    out <- DT[bin_ed == cond][
      , list(fit = list(robustlogrm(formula, data = .SD, ...))), by = value_bin_ed]#[
    #      value_bin_ed == 'blank', value_bin_ed := '']
    setnames(out, 'value_bin_ed', targetVarName)

  }

  return(out[])

}







