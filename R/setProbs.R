#' @title Set value of slot \code{probs} of an object \linkS4class{categObsPredModelParam}
#'
#' @description \code{setProbs} assigns a \linkS4class{data.table} to the slot \code{probs} of the
#' input object \linkS4class{categObsPredModelParam}.
#'
#' @param object Object whose slot \code{probs} is to be assigned.
#'
#' @param value \linkS4class{data.table} to be assigned to the slot \code{probs}.
#'
#' @return Object \linkS4class{categObsPredModelParam} with slot \code{probs} updated.
#'
#' @import data.table
#'
#' @rdname setProbs
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setGeneric("setProbs<-", function(object, value){standardGeneric("setProbs<-")})

#' @export
setReplaceMethod(
  f = "setProbs",
  signature = c("categObsPredModelParam","data.table"),
  function(object, value){

    object@probs <- value
    return(object)
  }
)
