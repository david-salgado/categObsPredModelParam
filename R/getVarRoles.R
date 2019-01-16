#' @title Return slot \code{VarRoles} from the input object
#'
#' @description \code{getVarRoles} extracts the slot \code{VarRoles} of the input object.
#'
#' @param object Object of class \linkS4class{categObsPredModelParam}.
#'
#' @return List with components \code{Units}, \code{Domains}, \code{DesignW}, \code{Regressands},
#' \code{Regressors}.
#'
#' @examples
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setGeneric("getVarRoles", function(object){standardGeneric("getVarRoles")})

#' @rdname getVarRoles
#'
#' @export
setMethod(
  f = "getVarRoles",
  signature = c("categObsPredModelParam"),
  function(object){object@VarRoles}
)
