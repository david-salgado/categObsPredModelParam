#' @title Compute error probabilities according to the error model
#'
#'
#' @param object Object of class \linkS4class{categObsPredModelParam}.
#'
#' @details Regarding \code{formula} see \code{\link{robustlogrm}}.
#'
#' The input data set \code{modelsDT} must contain the following colums:
#' \enumerate{
#'
#' \item LHS of \code{formula}: values of the target variable.
#'
#' \item \code{fit.pi}: glm model fit for the probabilities \eqn{\pi(x)}.
#'
#' \item \code{fit.1}: glm model fit for the probabilities \eqn{p(\cdot|1)}.
#'
#' \item \code{fit.0}: glm model fit for the probabilities \eqn{p(\cdot|0)}.
#'
#' }
#'
#' @return A \linkS4class{data.table} with the following columns:
#'
#' \enumerate{
#'
#' \item One per regressor in \code{formula}.
#'
#' \item Regressand in \code{formula}.
#'
#' \item \code{pi}: for probabilities \eqn{\pi(x)}. See (REF).
#'
#' \item \code{p11}: for probabilities \eqn{p(1|1)}. See (REF).
#'
#' \item \code{p01}: for probabilities \eqn{p(0|1)}. See (REF).
#'
#' \item \code{p10}: for probabilities \eqn{p(1|0)}. See (REF).
#'
#' \item \code{p00}: for probabilities \eqn{p(0|0)}. See (REF).
#'
#' \item \code{P11}: for probabilities \eqn{P(1|1)}. See (REF).
#'
#' \item \code{P01}: for probabilities \eqn{P(0|1)}. See (REF).
#'
#' \item \code{P10}: for probabilities \eqn{P(1|0)}. See (REF).
#'
#' \item \code{P00}: for probabilities \eqn{P(0|0)}. See (REF).

#'
#' }
#' @examples
#' \dontrun{
#' id.vars <- c('IDENTHOGAR', 'NORDEN')
#' formula <- 'CNO ~ CNAE1'
#' probs <- computeProbs(models, id.vars, formula)
#' }
#'
#' @import data.table StQ
#'
#' @include categObsPredModelParam-class.R getModelFits.R getRegressands.R getRegressors.R getData.R
#'
#' @export
setGeneric("computeProbs", function(object){standardGeneric("computeProbs")})

#' @rdname computeProbs
#'
#' @export
setMethod(
  f = "computeProbs",
  signature = c("categObsPredModelParam"),
  function(object){

  targetVarNames <- getRegressands(object)
  regressors <- getRegressors(object)
  id.vars <- object@VarRoles$Units
  outCols <- c(id.vars, regressors)

  probs.dt <- lapply(targetVarNames, function(targetVarName){

    allVars <- c(outCols, targetVarName)

    modelsDT <- getModelFits(object)

    data.pi <- modelsDT[, fit.pi[[1]]$data, by = targetVarName][, ..allVars][
      , pi := modelsDT[, list(pi = fit.pi[[1]]$fitted), by = targetVarName][['pi']]]
#return(data.pi)
    data.p1 <- modelsDT[
      , {if (!is.null(fit.1[[1]]) & !is.null(fit.pi[[1]])){

        coefs <- fit.1[[1]]$coefficients;
        #print('coefs:');
        #print(names(coefs));
        mat <- model.matrix(fit.1[[1]]$formula, fit.pi[[1]]$data);
        #print('mat:');
        #print(colnames(mat));
        mat <- mat[, names(coefs), drop = FALSE];
        logitPreds <- mat %*% coefs;
        preds <- exp(logitPreds) / (1 + exp(logitPreds));
        fit.pi[[1]]$data[, c(id.vars, regressors), with = FALSE][, p11 := preds]

      } 
      }, by = targetVarName]

    data.p0 <- modelsDT[
      , {if (!is.null(fit.0[[1]]) & !is.null(fit.pi[[1]])){
        coefs <- fit.0[[1]]$coefficients;
        mat <- model.matrix(fit.0[[1]]$formula, fit.pi[[1]]$data)[, names(coefs), drop = FALSE];
        logitPreds <- mat %*% coefs;
        preds <- exp(logitPreds) / (1 + exp(logitPreds));
        fit.pi[[1]]$data[, c(id.vars, regressors), with = FALSE][, p10 := preds]
      }
      }, by = targetVarName]

    data.prob <- merge(data.pi, data.p1, by = allVars, all = TRUE)
    data.prob[, p11 := ifelse(is.na(p11), 0, p11)][, p01 := 1 - p11]
    data.prob <- merge(data.prob, data.p0, by = allVars, all = TRUE)
    data.prob[, p10 := ifelse(is.na(p10), 0, p10)][, p00 := 1 - p10][
      , P00 := ( p00 * (1 - pi) ) / ( p00 * (1 - pi) + (1 - p11) * pi )][
      , P10 := 1 - P00][
      , P11 := ( p11 * pi ) / ( p11 * pi + (1 - p00) * (1 - pi) )][
      , P01 := 1 - P11]
    out <- data.prob[!duplicated(data.prob, by = c(regressors, targetVarName))][
      , (id.vars) := NULL]
    out[, variable := targetVarName]
    return(out[])
  })
return(probs.dt)
  probs.dt <- rbindlist(probs.dt)
  setcolorder(probs.dt, c('variable', targetVarNames, regressors,
                        'pi', 'p11', 'p01', 'p10', 'p00', 'P00', 'P10', 'P11', 'P01'))
  setProbs(object) <- probs.dt
  return(object)

})
