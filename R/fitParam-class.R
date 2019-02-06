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
#' @slot selection logical vector of length 1 to indicate whether a model selection is to be
#' performed (\code{TRUE}) or not (\code{FALSE}).
#'
#' @slot formula character vector with formula(s) for the model(s) to fit.
#'
#' @slot selParam list of parameters for the model selection when \code{selection == TRUE}
#'
#'
#' @examples
#' # An empty contObsPredModelParam object:
#' new(Class = 'fitParam')
#'
#' \dontrun{
#' source('R:/USIE/Proyecto_DepSel_VarQual/datos/PreparacionStQsAnalisis.R', encoding = 'UTF-8')
#' fitPar <- new(Class = 'fitParam', edData = ff_2011_model.StQ, rawData = fd_2011_model.StQ,
#'               selection = FALSE,  formula = 'Ocupacion_35.__11.1.3._ ~ ActivEcono_35.__2.1.1._',
#'               selParam = list())
#'
#' }
#'
#' @import data.table StQ
#'
#' @export
setClass(Class = "fitParam",
         slots = c(edData = 'StQ',
                   rawData = 'StQ',
                   selection = 'logical',
                   formula = 'character',
                   selParam = 'list'),
         prototype = list(edData = StQ::StQ(),
                          rawData = StQ::StQ(),
                          selection = FALSE,
                          formula = '',
                          VarRoles = list()),
         validity = function(object){

           selection <- slot(object, 'selection')
           if (selection == TRUE){

             params <- c('edEffInd', 'globalInd', 'priorBin')
             params.errorIndex <- which(!params %in% names(objects@selParam))
             if (length(params.errorIndex) > 0){

               stop(paste0('[fitParam validity] The following selection parameters are missing: ',
                           paste0(params[params.errorIndex], collapse = ', '), '.\n'))
             }

           }
           return(TRUE)
         }
)
