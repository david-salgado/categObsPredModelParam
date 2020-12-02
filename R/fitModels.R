#' @title Return \linkS4class{data.table} with model fits for probabilities.
#'
#' @description \code{fitModels} computes model fits for probabilities. 
#'
#' @param object Object of class \linkS4class{categObsPredModelParam}.
#'
#' @param fitParam Object of class \linkS4class{fitParam}.
#' 
#' @param na.as.category logical with default to \code{FALSE} to deal with NA values in the model. 
#' If \code{TRUE} the missing values are treated as a new category.
#'
#' @return Object of class \linkS4class{categObsPredModelParam} with the slot 
#' \code{modelFits}
#' computed with the fitted models.
#'
#' @examples
#'
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
#' }
#'
#' @include categObsPredModelParam-class.R fitParam-class.R getSelection.R 
#' getEdData.R getRawData.R setModelFits.R setTrain.R setTest.R RFfit.R
#'
#' @import data.table StQ
#'
#' @export
setGeneric("fitModels", function(object, fitParam, na.as.category){
  standardGeneric("fitModels")
  })

#' @rdname fitModels
#'
#' @export
setMethod(
  f = "fitModels",
  signature = c("categObsPredModelParam", "fitParam", "logical"),
  function(object, fitParam, na.as.category = FALSE){

  targetVars <- getRegressands(object)
  designWeight <- object@VarRoles$DesignW
  parms <- fitParam@selParam
  ptrain <- parms$ptrain
  regressors <- object@VarRoles$Regressors
  IdUnits <- object@VarRoles$Units
  
  # Dataset depurado  
  edData.StQ <- getEdData(fitParam)
  IDQuals_ed <- getIDQual(edData.StQ, 'MicroData')
 
  edData.dt <- dcast_StQ(edData.StQ, UnitNames = FALSE)
  edData.dt <- edData.dt[, c(IdUnits, targetVars), with = FALSE]
  setnames(edData.dt, targetVars, paste0(targetVars, '_ed'))

  # Dataset sin depurar
  rawData.StQ <- getRawData(fitParam)
  IDQuals_raw <- getIDQual(rawData.StQ, 'MicroData')

  rawData.dt <- dcast_StQ(rawData.StQ, UnitNames = FALSE)
  rawData.dt <- rawData.dt[, as.character(designWeight) 
                           := as.numeric(get(designWeight)) ]
    
 
  ### Construimos el conjunto de datos de análisis. 
  Data <- copy(rawData.dt)[!is.na(get(designWeight))]
  
  # Se introduce la variable depurada  
  Data <- merge(Data, edData.dt, by = IDQuals_ed, all.x = TRUE)
  
  if(na.as.category){
    # Valores NA se pasan a *
    for (j in seq_len(ncol(Data))){
      set(Data, which(is.na(Data[[j]])), j, '*')
    }
  }
  
  # Variables numéricas
  nums <- unlist(lapply(Data, is.numeric))
  nums <- colnames(Data)[nums]
  
  # Factores y se añaden las variables objetivo (target)
  targetVars_ed <- paste0(targetVars,'_ed')
  #targetVars_raw <-  paste0(targetVars,'_raw')
  
  for (j in 1:length(targetVars)){
  assign(paste0(targetVars[j],'_levels'),
         sort(union(unique(eval(parse(text=paste0('Data$',targetVars_ed[j] )))), 
                    unique(eval(parse(text=paste0('Data$',targetVars[j])))) )))
   
  Data[
      , targetVars_ed[j] := factor(get(targetVars_ed[j]), 
                          levels = get(paste0(targetVars[j],'_levels')))][
      , targetVars[j] := factor(get(targetVars[j]), 
                          levels = get(paste0(targetVars[j],'_levels')))][
      , paste0('target',j) := (get(targetVars[j]) != get(targetVars_ed[j]))*1][
      , paste0('target',j) := (get(targetVars[j]) != get(targetVars_ed[j]))*1][  
      ]   
   
  }
  
  cols <- names(Data[, setdiff(names(Data), 
                    c(targetVars_ed,targetVars,IDQuals_ed, nums)), with=FALSE])
  
  Data[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
  
 
  # Se ajusta un random forest (train -> 80%; test -> 20%)
  id_train <- sample(1:nrow(Data), size = floor(ptrain * nrow(Data)))
  train <- Data[id_train, ]
  test <- Data[-id_train,]
  
  listTest <- list(data = test)
  listTrain <- list(data = train)
  
  setTest(object) <- listTest
  setTrain(object) <- listTrain
  
  model <- list()
  
  for (j in 1:length(targetVars)) {
      print(paste0('Training model ' ,j,' ...'))
    
      targetVarName <- paste0('target',j)
      
      mod <- RFfit(data = train, 
               targetVarName = targetVarName,
               regressors = regressors,
               parms=parms)
      
      print(class(mod))
      model <- append(model,list(mod))
      print(class(model))
      print(class(model[[j]]))
      #assign(paste0("model",j),model)
  }
  
  names(model) <- c(targetVars)
  
  setModelFits(object) <- model
  
  return(object)
  
  
  }
)

