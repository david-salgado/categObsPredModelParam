#' @title Set value of component \code{Regressands} of an object \linkS4class{categObsPredModelParam}
#'
#' @description \code{setRegressands} assigns a character value to the component \code{Regressands}
#' of the slot \code{VarRoles} of the input object \linkS4class{categObsPredModelParam}.
#'
#' @param object Object \linkS4class{categObsPredModelParam} whose component \code{Regressands}
#' is to be assigned.
#'
#' @param value character vector to be assigned to the component \code{Regressands}.
#'
#' @return Object \linkS4class{categObsPredModelParam} with the component \code{Regressands} updated.
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setGeneric("setRegressands<-", function(object, value){standardGeneric("setRegressands<-")})
#'
#' @export
setReplaceMethod(
  f = "setRegressands",
  signature = c("categObsPredModelParam", "character"),
  function(object, value){

    object@VarRoles$Regressands <- value
    validObject(object)
    return(object)
  }
)
