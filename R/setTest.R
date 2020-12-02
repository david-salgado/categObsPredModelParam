#' @title Set value of slot \code{Test} of an object \linkS4class{categObsPredModelParam}
#'
#' @description \code{setTest} assigns a \code{list} to the slot \code{Test} of the input
#' object \linkS4class{categObsPredModelParam}
#'
#' @param object Object whose slot \code{Test} is to be assigned.
#'
#' @param value Object of class \code{list} to be assigned to the slot \code{Test}.
#'
#' @return Object \linkS4class{categObsPredModelParam} with slot \code{Test} updated.
#'
#' @import data.table
#'
#' @rdname setTest
#'
#' @include categObsPredModelParam-class.R
#' 
#' @export
setGeneric("setTest<-", function(object, value){standardGeneric("setTest<-")})
#'
#' @import data.table
#'
#' @rdname setTest
#'
#' @include categObsPredModelParam-class.R
#' 
#' @export
setReplaceMethod(
  f = "setTest",
  signature = c("categObsPredModelParam","list"),
  function(object, value){
    
    object@Test <- value
    return(object)
  }
)
