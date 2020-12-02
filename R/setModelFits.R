#' @title Set value of slot \code{modelFits} of an object \linkS4class{categObsPredModelParam}
#' 
#' @description \code{setModelFits} assigns a \linkS4class{data.table} to the slot \code{modelFits}
#' of the input object \linkS4class{categObsPredModelParam} with the fitted models for the
#' probabilities in the categorical observation-prediction model.
#'
#' @param object Object whose slot \code{modelFits} is to be assigned.
#'
#' @param value Object of class \code{data.table} to be assigned to the slot \code{modelFits}.
#'
#' @return Object \linkS4class{categObsPredModelParam} with slot \code{modelFits} updated.
#'
#' @import data.table
#' 
#' @docType methods
#' 
#' @rdname setModelFits
#' 
#' @importFrom methods validObject
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setGeneric("setModelFits<-", function(object, value){standardGeneric("setModelFits<-")})

#' @rdname setModelFits
#' 
#' @importFrom methods validObject
#' @export
setReplaceMethod(
  f = "setModelFits",
  signature = c("categObsPredModelParam","list"),
  function(object, value){

    object@modelFits <- value
    validObject(object)
    return(object)
  }
)
