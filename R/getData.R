#' @title Return slot \code{Data} from the input object
#'
#' @description \code{getData} extracts the slot \code{Data} which contains the parameters of a
#' categorical observation-prediction model or all data necessary to compute them of the input object.
#'
#' @param object Object of class \linkS4class{categObsPredModelParam}.
#'
#' @return Object of class \linkS4class{StQ} corresponding to the slot \code{Data} of the input
#' parameter.
#'
#' @examples
#'
#' @import StQ
#'
#' @rdname getData
#'
#' @include categObsPredModelParam-class.R
#'
#' @export
setMethod(
  f = "getData",
  signature = c("categObsPredModelParam"),
  function(object){object@Data}
)
