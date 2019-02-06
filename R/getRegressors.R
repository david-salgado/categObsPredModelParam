#' @title Return the regressors for the meaurement error models from the input object
#'
#' @description \code{getRegressors} extracts the component \code{Regressors} from the slot
#'  \code{VarRoles} of the input object.
#'
#' @param object Object of class \linkS4class{categObsPredModelParam}.
#'
#' @return Character vector with the names of the regressors.
#'
#' @examples
#' \dontrun{
#' source('R:/USIE/Proyecto_DepSel_VarQual/datos/PreparacionStQsAnalisis.R', encoding = 'UTF-8')
#' fitPar <- new(Class = 'fitParam', edData = ff_2011_model.StQ, rawData = fd_2011_model.StQ,
#'               selection = FALSE,  formula = 'Ocupacion_35.__11.1.3._ ~ ActivEcono_35.__2.1.1._',
#'               selParam = list())
#' getEdData(fitPar)
#'}
#'
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setGeneric("getRegressors", function(object){standardGeneric("getRegressors")})

#' @rdname getRegressors
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setMethod(
  f = "getRegressors",
  signature = c("categObsPredModelParam"),
  function(object){object@VarRoles[['Regressors']]}
)
