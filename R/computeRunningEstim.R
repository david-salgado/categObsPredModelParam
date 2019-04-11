#' @title Return a \linkS4class{data.table}) with the sum of desing weights that remain
#' to be explained, according to the number of edited units.
#'
#' @description \code{computeRunningEstim} computes the sum of design weights
#' for every value in \code{edPriority}, according to the following procedure:
#' In each row of the returning \linkS4class{data.table}), the variable edPriority
#' indicates the number of edited units. The variable estim contains the sum of 
#' design weights without taking into account design weights of edited units. 
#' They are set to 0.
#' 
#' @param edPriority mesh of points to calculate the pseudo relative bias. These values 
#' represent the number of edited units.
#'
#' @param data data frame, list or environment (or object coercible by
#' \code{as.data.table} to a \linkS4class{data.table}) containing the variables
#' in the model (see 'Details').
#' 
#' @param levelsTargetVar every values of the regresand (an object of class character).
#' 
#' @param targetVar name of the regressand (an object of class character).
#' 
#' @param id.vars names of the identification variables for each unit in the
#' input data set \code{data}.
#' 
#' @param designWeight an object of class character containing the design weight.
#' 
#' @param suffix parameter for the name of the edited version of the variable under analysis.
#' 
#' @return \linkS4class{data.table} with three columns: 
#' \code{targetVar} with the values of regressand, estim with the sum of desing weights 
#' that remain to be explained, according to the number of edited units (variable edPriority).
#'
#' @examples
#'
#' \dontrun{
#' }
#'
#' @include 
#'
#' @import data.table
#'
#' @export
#' 

computeRunningEstim <- function(edPriority, data, levelsTargetVar, targetVar, id.vars,
                                designWeight, suffix = '_ed'){
  
  if (class(data[[targetVar]]) %in% c('character', 'factor')){
  
  
    #levelsTargetVar <- levels(data[[targetVar]])

    workingDT <- copy(data)[, (levelsTargetVar) := lapply(levelsTargetVar, function(val){get(targetVar) == val})]
    DT <- melt(workingDT, 
               id.vars = c(id.vars, targetVar, paste0(targetVar, suffix), designWeight, 'P01', 'moment', 'priority'), 
               measure.vars = levelsTargetVar, variable.name = 'value_bin',
               value.name = 'bin')
    workingDT_ed <- copy(data)[, (levelsTargetVar) := lapply(levelsTargetVar, function(val){get(paste0(targetVar, suffix)) == val})]
    DT_ed <- melt(workingDT_ed, 
                  id.vars = c(id.vars, targetVar, paste0(targetVar, suffix), designWeight, 'P01', 'moment', 'priority'), 
                  measure.vars = levelsTargetVar, variable.name = 'value_bin_ed',
                  value.name = 'bin_ed')
    
    DT[, value_bin_ed := DT_ed[['value_bin_ed']]][
      , bin_ed := DT_ed[['bin_ed']]][
        bin == TRUE]
    
    if (length(edPriority) == 1){
      
      DT[, bin_run := bin][
        priority <= edPriority, bin_run := bin_ed]
      output <- DT[, list(estim = sum(as.numeric(get(designWeight)) * bin_run, na.rm = TRUE)), by = c('value_bin')]
      setnames(output, 'value_bin', targetVar)[, edPriority := edPriority]
      return(output)    
      
    } else {
      
      output <- lapply(edPriority, function(edPrior){
        
        DT[, bin_run := bin][
          priority <= edPrior, bin_run := bin_ed]
        localOutput <- DT[, list(estim = sum(as.numeric(get(designWeight)) * bin_run, na.rm = TRUE)), by = c('value_bin')]
        setnames(localOutput, 'value_bin', targetVar)[, edPriority := edPrior]
        return(localOutput)
        
      })
      output <- rbindlist(output)
      return(output)
      
    }
  }
}
