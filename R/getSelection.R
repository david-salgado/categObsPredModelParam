#' @title Return slot \code{selection} from the input object
#'
#' @description \code{getSelection} extracts the slot \code{selection} of the input object.
#'
#' @param object Object of class \linkS4class{fitParam}.
#'
#' @return Logical indicating whether a model selection is requested or not in the parameters for
#' the fit of the model.
#'
#' @examples
#' \dontrun{
#' source('R:/USIE/Proyecto_DepSel_VarQual/datos/PreparacionStQsAnalisis.R', encoding = 'UTF-8')
#' fitPar <- new(Class = 'fitParam', edData = ff_2011_model.StQ, rawData = fd_2011_model.StQ,
#'               selection = FALSE,  formula = 'Ocupacion_35.__11.1.3._ ~ ActivEcono_35.__2.1.1._',
#'               selParam = list())
#' getSelection(fitPar)
#'}
#'
#' @include fitParam-class.R
#'
#' @export
setGeneric("getSelection", function(object){standardGeneric("getSelection")})

#' @rdname getSelection
#'
#' @export
setMethod(
  f = "getSelection",
  signature = c("fitParam"),
  function(object){object@selection}
)
