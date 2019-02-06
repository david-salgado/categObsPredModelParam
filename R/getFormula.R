#' @title Return slot \code{formula} from the input object
#'
#' @description \code{getFormula} extracts the slot \code{formula} of the input object.
#'
#' @param object Object of class \linkS4class{fitParam}.
#'
#' @return Object of class \code{formula} with the formula for models.
#'
#' @examples
#' \dontrun{
#' source('R:/USIE/Proyecto_DepSel_VarQual/datos/PreparacionStQsAnalisis.R', encoding = 'UTF-8')
#' fitPar <- new(Class = 'fitParam', edData = ff_2011_model.StQ, rawData = fd_2011_model.StQ,
#'               selection = FALSE,  formula = 'Ocupacion_35.__11.1.3._ ~ ActivEcono_35.__2.1.1._',
#'               selParam = list())
#' getFormula(fitPar)
#'}
#'
#' @include fitParam-class.R
#'
#' @export
setGeneric("getFormula", function(object){standardGeneric("getFormula")})

#' @rdname getFormula
#'
#' @export
setMethod(
  f = "getFormula",
  signature = c("fitParam"),
  function(object){as.formula(object@formula)}
)
