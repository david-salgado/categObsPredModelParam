#' @title Set value of slot \code{Train} of an object \linkS4class{categObsPredModelParam}
#'
#' @description \code{setTrain} assigns a \code{list} to the slot \code{Train} of the input
#' object \linkS4class{categObsPredModelParam}
#'
#' @param object Object whose slot \code{Train} is to be assigned.
#'
#' @param value Object of class \code{list} to be assigned to the slot \code{Train}.
#'
#' @return Object \linkS4class{categObsPredModelParam} with slot \code{Train} updated.
#'
#' @import data.table
#'
#' @rdname setTrain
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setGeneric("setTrain<-", function(object, value){standardGeneric("setTrain<-")})
#'
#' @import data.table
#'
#' @rdname setTrain
#'
#' @include categObsPredModelParam-class.R
#' 
#' @export
setReplaceMethod(
  f = "setTrain",
  signature = c("categObsPredModelParam","list"),
  function(object, value){
    
    object@Train <- value
    return(object)
  }
)
