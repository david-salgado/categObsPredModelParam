#' @title Set value of slot \code{Data} of an object \linkS4class{categObsPredModelParam}
#'
#' @description \code{setData} assigns a \code{StQ} to the slot \code{Data} of the input
#' object \linkS4class{categObsPredModelParam}
#'
#' @param object Object whose slot \code{Data} is to be assigned.
#'
#' @param value Object of class \code{StQ} to be assigned to the slot \code{Data}.
#'
#' @return Object \linkS4class{categObsPredModelParam} with slot \code{Data} updated.
#'
#' @import StQ
#'
#' @rdname setData
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setReplaceMethod(
  f = "setData",
  signature = c("categObsPredModelParam","StQ"),
  function(object, value){

    object@Data <- value
    return(object)
  }
)
