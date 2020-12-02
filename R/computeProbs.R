#' @title Compute error probabilities according to the error model and 
#' units ordered by priority
#'
#' @param object Object of class \linkS4class{categObsPredModelParam}.
#'
#' @param na.as.category logical with default to \code{FALSE} to deal with NA values in the model. 
#' If \code{TRUE} the missing values are treated as a new category.
#' 
#' @return A \linkS4class{categObsPredModelParam} with  the new slot probs with the following columns:
#'
#' \enumerate{
#' 
#' \item Identification variables for each unit in the input data set
#'
#' \item One per regressor 
#'
#' \item Error probabilities
#'
#' \item The priority based on error probabilities for each of units
#' 
#' \item Error moments
#' 
#' \item The priority based on error moments for each of units
#'
#'
#' }
#' @examples
#' \dontrun{
#' object <- readRDS("R:/USIE/Proyecto_DepSel_VarQual/categ_RF_simulacion/PC/RDS/ObsPredPar1.rds")
#' poolDD <- getDD(object@Data)
#' regressors <- getRegressors(object)  
#' targetVars <- getRegressands(object)
#' id.vars <- object@VarRoles$Units  
#' vars <- c(id.vars,targetVars,regressors)
#' data.dt <- object@Test$data
#' data.dt[c(1:3,10:16), NivelEstudios_35._2.1.4.:= NA ] 
#' data.dt[c(4:7,17:22), Edad_05._2.1.5.1.:= NA]        
#' # Provocamos un nivel distinto en "SitEmpleo_35._2.1.5.1._"  
#' data.dt[c(30:49), SitEmpleo_35._2.1.5.1._ := rep(c('8', '9'), 10)] 
#' data.dt[c(23:35), Sexo_04._2.1.5.1. := '4'] 
#' data.StQ <- melt_StQ(data.dt[,..vars], poolDD)
#' object@Data <- data.StQ
#' object2 <- computeProbs(object)
#'   
#'   # Dividimos el conjunto de datos en tres: 
#'   # (i) con missing en alguna variable numérica, 
#'   # (ii) con niveles nuevos en alguna variable categórica
#'   # (iii) el resto, donde se imputa NAs a valor *
#'   # (i) y (ii) se imputan probError a 1
#'   # (iii) se aplica el modelo ajustado y se computan probabilidades
#'   # Se vuelven a combinar los datsets y se computan momentos de error y prioridades
#'
#'   # Provocamos missing en  "NivelEstudios_35._2.1.4." y "Edad_05._2.1.5.1."  
#' currentData.dt[c(1:3,10:16), NivelEstudios_35._2.1.4.:=NA] 
#' currentData.dt[c(4:7,17:22), Edad_05._2.1.5.1.:=NA]        
#'   # Provocamos un nivel distinto en "SitEmpleo_35._2.1.5.1._"  
#' currentData.dt[c(30:49), SitEmpleo_35._2.1.5.1._:='8'] 
#' }
#'
#' @import data.table StQ stats
#'
#' @include categObsPredModelParam-class.R getModelFits.R getRegressands.R getRegressors.R getData.R
#'
#' @export
setGeneric("computeProbs", function(object, na.as.category){standardGeneric("computeProbs")})

#' @rdname computeProbs
#'
#' @export
setMethod(
  f = "computeProbs",
  signature = c("categObsPredModelParam", "logical"),
  function(object, na.as.category = FALSE){
    
    '..vars' <- '..nums' <- '..factorsTrain' <- '..factorsCurrent' <- NULL
  
  targetVars <- getRegressands(object)
  designWeight <- object@VarRoles$DesignW
  regressors <- object@VarRoles$Regressors
  IDQuals <- object@VarRoles$Units 
  Train <- object@Train$data

  currentData.StQ <- object@Data
  currentData.dt <- dcast_StQ(currentData.StQ, UnitNames = FALSE)
  
  vars <- unique(c(IDQuals, regressors, targetVars))
  currentData.dt <- currentData.dt[, ..vars]
  setnames(currentData.dt, old=designWeight, new=c("designWeight"))
  currentData.dt <- currentData.dt[, designWeight := as.numeric(designWeight)]
  
  Train <- Train[, ..vars]
  setnames(Train, old=designWeight, new=c("designWeight"))
  Train <- Train[, designWeight := as.numeric(designWeight)]
  
  regressors <- c(regressors[-which(regressors==designWeight)], "designWeight")
  
  # Variables numéricas
  nums <- colnames(currentData.dt)[sapply(currentData.dt, is.numeric)]
  
  # En caso de que existan registros missing en alguna de las variables numéricas, se eliminan, ya que 
  # el modelo no les asignaría una probabilidad de error, pues la función predict daría error.
  missing.dt <- currentData.dt[!complete.cases(currentData.dt[, ..nums])]
  currentData.dt <- na.omit(currentData.dt, cols=c(nums))
  
  if(na.as.category){
    # Valores NA se pasan a *
    for (j in seq_len(ncol(currentData.dt))){
      set(currentData.dt, which(is.na(currentData.dt[[j]])), j, '*')
    }
  }
  
  # Convertimos las variables que no son numéricas a factores
  cols <- names(currentData.dt[, setdiff(names(currentData.dt), c(IDQuals, nums)), with=FALSE])
  currentData.dt[, (cols) := lapply(.SD, as.factor), .SDcols=cols]

  # Comprobamos si existe alguna variable con niveles distintos en currentData a los que tenía Train      CODIGO NUEVO
  factorsTrain <- names(Train)[sapply(Train, is.factor)]
  levelsTrain <- lapply(Train[, ..factorsTrain], levels)
  factorsCurrent <- names(currentData.dt)[sapply(currentData.dt, is.factor)]
  levelsCurrent <- lapply(currentData.dt[, ..factorsCurrent], levels)
  levelsCurrent <- levelsCurrent[names(levelsTrain)]
  difLevels <- lapply(seq(along = levelsCurrent), function(i){setdiff(levelsCurrent[[i]], levelsTrain[[i]])})
  names(difLevels) <- names(levelsCurrent)
  difLevels <- difLevels[sapply(difLevels, function(x){length(x) > 0})]
  if(length(difLevels) > 0){
    distintos <- rbindlist(lapply(seq(along = difLevels), function(i){currentData.dt[as.data.table(difLevels[i]), on = names(difLevels[i])]}))
    distintos <- distintos[!duplicated(distintos, by = IDQuals)]
    missing.dt <- rbind(missing.dt, distintos)
    currentData.dt <- currentData.dt[!distintos, on = IDQuals]
    currentData.dt <- droplevels(currentData.dt) # actualizamos los levels en currentData
  }

  

  setnames(currentData.dt, old=c("designWeight"), new=designWeight)
  setnames(Train, old=c("designWeight"), new=designWeight)
  if (nrow(missing.dt) > 0){
    setnames(missing.dt, old=c("designWeight"), new=designWeight)
  }
  # Homogeneizamos los levels de las variables de currentData con los del conjunto de Train 
  currentData.dt <- rbind(Train[1, ] , currentData.dt)
  currentData.dt <- currentData.dt[-1,]
  for (j in 1:length(targetVars)){ # para cada targetVars
    # Cálculo de las probabilidades
    model <- eval(parse(text=paste0('object@modelFits$',targetVars[j])))
    prob_error <- predict(model, currentData.dt, type = "prob")
    #name <- paste0('prob_error',j)
    currentData.dt <- currentData.dt[, paste0('prob_error',j) := prob_error[, 2]]
    rm(model)
    rm(prob_error)
    if (nrow(missing.dt) > 0){
      missing.dt <- missing.dt[, paste0('prob_error',j) := 1]
      # Juntamos ambos conjuntos
      currentData.dt <- rbind(currentData.dt, missing.dt)
    }
  }
  
  
  for (j in 1:length(targetVars)){
    # Cálculo de los momentos de error y ordenación de unidades
    currentData.dt <- currentData.dt[, paste0('prob_errorm',j) :=  get(paste0('prob_error',j)) * ifelse(is.na(get(designWeight)),9999,get(designWeight))]
    currentData.dt <- currentData.dt[order(-get(paste0('prob_error',j)))][, paste0('priority',j) := .I]
    currentData.dt <- currentData.dt[order(-get(paste0('prob_errorm',j)))][, paste0('prioritym',j) := .I]
  }
  
  
  
  setProbs(object) <- currentData.dt
  
  return(object)
  
})
