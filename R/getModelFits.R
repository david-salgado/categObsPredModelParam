#' @title Return slot \code{modelFits} from the input object
#'
#' @description \code{getModelFits} extracts the slot \code{VarRoles} of the input object.
#'
#' @param object Object of class \linkS4class{categObsPredModelParam}.
#'
#' @return \linkS4class{data.table} with the fitted models.
#'
#' @examples
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setGeneric("getModelFits", function(object){standardGeneric("getModelFits")})

#' @rdname getModelFits
#'
#' @export
setMethod(
  f = "getModelFits",
  signature = c("categObsPredModelParam"),
  function(object){object@modelFits}
)
