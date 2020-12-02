#' @title Return slot \code{Data} from the input object
#'
#' @description \code{getData} extracts the slot \code{Data} which contains the parameters of a
#' categorical observation-prediction model or all data necessary to compute them of the input object.
#'
#' @param object Object of class \code{"\linkS4class{categObsPredModelParam}"}.
#'
#' @return Object of class \code{"\link[StQ]{StQ}"} corresponding to the slot \code{Data} of the input
#' parameter.
#'
#' @examples
#' \dontrun{
#'   fitPar <- new(Class = 'fitParam',
#'   edData = FFall_AS.StQ, rawData = FGall_AS.StQ, 
#'   selParam = list(ntreeTry=1000, stepFactor=2, improve=0.05, 
#'                   trace=TRUE, plot=TRUE, doBest = TRUE, 
#'                   ptrain = 0.8, DD = DDactu),
#'                   valParam = list(edEffInd = effInd, priorBin = 5, 
#'                   dataVal = c('Train','Test')))
#'                   
#'  ObsPredPar1 <- new(Class = 'categObsPredModelParam',
#'                   Data = FGall_AS.StQ,
#'                   VarRoles = list(Units = IDUnits,
#'                   Domains = character(0),
#'                   DesignW = DesignW,
#'                   Regressands = Regressands,
#'                   Regressors = Regressors
#'                   ))
#'                   
#' ObsPredPar1 <-  fitModels(ObsPredPar1, fitPar, na.as.category)
#' getData(ObsPrePar1)
#' }
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
