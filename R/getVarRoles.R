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
#' getVarRoles(ObsPrePar1)
#' }

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
