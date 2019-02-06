#' @title Fit a robust logistic regression model
#'
#' @description This function is basically a wrapper for function
#' \code{\link[glm2]{glm2}} of package \code{glm2}. When a regressor is found to
#'  have only one value (thus producing the 'contrasts error' -- see e.g.
#' \href{https://stackoverflow.com/questions/44200195/how-to-debug-contrasts-can-be-applied-only-to-factors-with-2-or-more-levels-er?rq=1}{stackoverflow}).
#'
#' When there more than one regressor with only one value in the data set, an
#' error is returned prompting for manual check. When only one regressor is
#' found to have one value, the model is again fitted dropping the intercept.
#'
#' @param formula an object of class \code{\link[stats]{formula}} (or one that
#' can be coerced to that class): a symbolic description of the model to be
#' fitted. The details of model specification are given under ‘Details’.
#'
#' @param data an optional data frame, list or environment (or object coercible
#' by \code{\link{as.data.frame}} to a data frame) containing the variables in
#' the model. If not found in data, the variables are taken from
#' \code{environment(formula)}, typically the environment from which
#' \code{robustlogrm} is called.
#'
#' @param ...	extra arguments to be used to form the default control argument.
#'
#' @details A typical predictor has the form response ~ terms where response is
#' the (numeric) response vector and terms is a series of terms which specifies
#' a linear predictor for response. The response can also be specified as a
#' factor or as a two-column matrix with the columns giving the numbers of
#' successes and failures. A terms specification of the form first + second
#' indicates all the terms in first together with all the terms in second with
#' any duplicates removed.
#'
#' A specification of the form first:second indicates the set of terms obtained
#' by taking the interactions of all terms in first with all terms in second.
#' The specification first*second indicates the cross of first and second. This
#' is the same as first + second + first:second.
#'
#'
#' @return The value returned by \code{robustlogrm} has exactly the same
#' documentation as the value returned by \link[stats]{glm}, except for:
#' \code{method},	the name of the fitter function used, which by default is
#' \code{glm.fit2}.
#'
#' @examples
#' \dontrun{
#' ExcelName <- 'T:/E30163/E30163.NombresVariables_V1.xlsx'
#' ValidateXLS(ExcelName)
#' }
#'
#' @import data.table
#'
#' @export
robustlogrm <- function(formula, data, ...){

  data <- copy(as.data.table(data))
  varNames <- all.vars(as.formula(formula))
  targetVarName <- varNames[1]
  regressors <- setdiff(varNames, targetVarName)


  out <- try(glm2::glm2(formula, data = data, family = 'binomial', ...), silent = TRUE)

  if (class(out)[1] == 'try-error') {

    if (dim(data)[1] == 1) return(NULL)
    regressorsNValues <- sapply(data[, ..regressors], function(x){length(unique(x))})
    names(regressorsNValues) <- regressors
    if (length(regressorsNValues) > 1) {

      degenRegressors <- names(regressorsNValues[regressorsNValues == 1])
      nonDegenRegressors <- setdiff(regressors, degenRegressors)
      if (length(nonDegenRegressors) == 0) {

        newForm <- as.formula(paste0(targetVarName, ' ~ ', degenRegressors[1]))

      } else {

        newForm <- as.formula(
          paste0(targetVarName, ' ~ ',
                 paste0(nonDegenRegressors, collapse = '+')))

      }
      out <- robustlogrm(newForm, data, ...)
      return(out)

    } else {

      degenRegressors <- names(regressorsNValues[regressorsNValues == 1])
      nonDegenRegressors <- setdiff(regressors, degenRegressors)
      if (length(degenRegressors) == 1) {

        newForm <- as.formula(gsub('~ ', '~ 0 + ', deparse(formula)))
        colOrder <- copy(names(data))
        degenRegValue <- unique(data[[degenRegressors]])
        data[, (degenRegressors) := NULL][, newVar := 1L]
        setnames(data, 'newVar', degenRegressors)
        setcolorder(data, colOrder)
        out <- glm2::glm2(newForm, data = data, family = 'binomial', ...)
        names(out$coefficients) <- paste0(names(out$coefficients), degenRegValue)


      } else {

        warning('[robustlogrm] Error: original data return')
        return(list(formula, regressorsNValues, data))

      }
    }
  }
  return(out)



}
