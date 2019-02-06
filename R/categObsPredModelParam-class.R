#' @title S4 class for the parameters of a categorical observation-prediction model
#'
#' @description Definition of the S4 class named \code{categObsPredModelParam} for the parameters of
#' a categorical observation-prediction model in the optimization approach to selective editing.
#'
#'
#' @slot Data Object of S3 class \code{StQ} with the parameters or all data necessary to
#'  compute them.
#'
#' @slot VarRoles List with components \code{Units}, \code{Domains}, \code{DesignW},
#'       \code{Regressands}, \code{Regressors} being character vectors containing the column names
#'       according to their respective role in the model.
#'
#'
#' @examples
#' # An empty contObsPredModelParam object:
#' new(Class = 'categObsPredModelParam')
#'
#' \dontrun{
#'
#' ObsPredPar <- new(Class = 'categObsPredModelParam',
#'                   Data = FD,
#'                   VarRoles = list(Units = 'NOrden', Domains = 'GeoLoc_35._4._2.1.5._1.2.3.'))
#'
#' }
#'
#' @import data.table StQ
#'
#' @export
setClass(Class = "categObsPredModelParam",
         slots = c(Data = 'StQ',
                   VarRoles = 'list',
                   modelFits = 'data.table',
                   probs = 'data.table'),
         prototype = list(Data = StQ::StQ(),
                          VarRoles = list(Units = character(0),
                                          Domains = character(0),
                                          DesignW = character(0),
                                          Regressands = character(0),
                                          Regressors = character(0)),
                          modelFits = data.table::data.table(),
                          probs = data.table::data.table()),
         validity = function(object){

           VarRoles <- slot(object, 'VarRoles')
           if (!all(names(VarRoles) %in% c('Units', 'Domains', 'DesignW', 'Regressands', 'Regressors'))){

             stop('[categObsPredModelParam: validity] All components of VarRoles must be one of these: Units, Domains, DesignW, Regressands, Regressors.')

           }


           Variables <- unlist(VarRoles)
           slotData <- slot(object, 'Data')
           Data <- dcast_StQ(slotData, setdiff(ExtractNames(Variables), getIDQual(slotData)))
           DataColNames <- names(Data)
           VarNotinData <- setdiff(Variables, DataColNames)

           if (length(VarNotinData) != 0) {

             stop(paste0('[categObsPredModelParam: validity] The following variables in VarRoles are not in the slot Data:\n',
                          paste0(VarNotinData, collapse = ', '), '.\n'))
           }

           return(TRUE)
         }
)
