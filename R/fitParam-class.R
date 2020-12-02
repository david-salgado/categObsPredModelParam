#' @title S4 class for the parameters necessary to fit a categorical observation-prediction model
#'
#' @description Definition of the S4 class named \code{fitParam} for the parameters needed to fit
#' a categorical observation-prediction model in the optimization approach to selective editing.
#'
#'
#' @slot edData object of class \code{\link[StQ]{StQ}} with the edited and validated values.
#'
#' @slot rawData object of class \code{\link[StQ]{StQ}} with the raw values.
#'
#' @slot selParam list of parameters for the model selection when \code{selection == TRUE}
#' @slot valParam list of parameters for validation.
#'
#' @examples
#' # An empty contObsPredModelParam object:
#' new(Class = 'fitParam')
#'
#' \dontrun{
#' source('R:/USIE/Proyecto_DepSel_VarQual/datos/PreparacionStQsAnalisis.R', encoding = 'UTF-8')
#' fitPar <- new(Class = 'fitParam', edData = ff_2011_model.StQ, rawData = fd_2011_model.StQ,
#'              selParam = list())
#'
#' }
#'
#' @include effInd.R
#' 
#' @import data.table StQ
#'
#' @export
setClass(Class = "fitParam",
         slots = c(edData = 'StQ',
                   rawData = 'StQ',
                   selParam = 'list',
                   valParam = 'list'),
         prototype = list(edData = StQ::StQ(),
                          rawData = StQ::StQ()),
         validity = function(object){

             params <- c('ntreeTry', 'stepFactor', 'improve', 'trace', 'plot', 'doBest', 'ptrain')
             params.errorIndex <- which(!params %in% names(object@selParam))
             if (length(params.errorIndex) > 0){

               stop(paste0('[fitParam validity] The following selection parameters are missing: ',
                           paste0(params[params.errorIndex], collapse = ', '), '.\n'))
             }

             valParams <- c('edEffInd', 'priorBin', 'dataVal')
             valParams.errorIndex <- which(!valParams %in% names(object@valParam))
             if (length(valParams.errorIndex) > 0){
               
               stop(paste0('[fitParam validity] The following validation parameters are missing: ',
                           paste0(valParams[valParams.errorIndex], collapse = ', '), '.\n'))
             }
           
           return(TRUE)
         }
)
