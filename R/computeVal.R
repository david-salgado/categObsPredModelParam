#' @title Return model validation for an input data setbased on a specific efficiency indicator () 
#' and AUC - ROC Curve
#'                                                                   
#' @description \code{computeVal} computes an efficiency indicator based on the absolute relative
#'  pseudo-bias, and AUC - ROC Curve for the training or test sample, in order to evaluate the model
#'  in slot \code{modelFits}.
#'
#' @param object Object of class \linkS4class{categObsPredModelParam}.
#'
#' @param fitParam Object of class \linkS4class{fitParam}.
#' 
#' @param na.as.category logical with default to \code{FALSE} to deal with NA values in the model. 
#' If \code{TRUE} the missing values are treated as a new category.
#'
#' @return Object of class \linkS4class{categObsPredModelParam} with the slot \code{Train} or 
#' slot \code{Test} updated with the efficiency indicator and AUC - ROC Curve.
#'
#' @examples
#'
#' \dontrun{
#' path <- 'R:/USIE/Proyecto_DepSel_VarQual'
#' preDD <- RepoReadWrite::RepoXLSToDD(file.path(p ath, 'E54009/E54009.NombresVariables_V1.xlsx'))
#' preFD.StQ <- RepoReadWrite::ReadRepoFile(file.path(path, 'E54009/E54009.FD_V1.AA2011.P_1'), 
#' preDD, perl = TRUE)
#' preFD.dt <- dcast_StQ(preFD.StQ)
#' setnames(preFD.dt, IDDDToUnitNames(names(preFD.dt), preDD))
#' preFD_AS.dt <- preFD.dt[
#'   , CNO_PR := ifelse(!is.na(B16_2), B16_2, NA)][
#'   , CNO_PR := ifelse(!is.na(B20a_2), B20a_2, CNO_PR)][
#'   , CNO_PR := ifelse(!is.na(B20m_2), B20m_2, CNO_PR)][
#'   , CNO_AS := ifelse(!is.na(F8_2), F8_2, NA)][
#'   , CNO_AS := ifelse(!is.na(F17a_2), F17a_2, CNO_AS)][
#'   , CNO_AS := ifelse(!is.na(F17m_2), F17m_2, CNO_AS)][
#'   , CNAE_PR := ifelse(!is.na(B15_2), B15_2, NA)][
#'   , CNAE_PR := ifelse(!is.na(B19a_2), B19a_2, CNAE_PR)][
#'   , CNAE_PR := ifelse(!is.na(B19m_2), B19m_2, CNAE_PR)][
#'   , CNAE_AS := ifelse(!is.na(F7_2), F7_2, NA)][
#'   , CNAE_AS := ifelse(!is.na(F16a_2), F16a_2, CNAE_AS)][
#'   , CNAE_AS := ifelse(!is.na(F16m_2), F16m_2, CNAE_AS)][
#'   , SitProf := ifelse(!is.na(B21a), B21a, NA)][
#'   , SitProf := ifelse(!is.na(B21m), B21m, SitProf)][
#'   , SitProf := ifelse(!is.na(B17), B17, SitProf)][
#'   , A11_i := ifelse(A11_1_i == '1', 1, NA)][
#'   , A11_i := ifelse(A11_2_i == '1', 2, A11_i)][
#'   , A11_i := ifelse(A11_3_i == '1', 3, A11_i)][
#'   , A11_i := ifelse(A11_4_i == '1', 4, A11_i)][
#'   , A11_i := ifelse(A11_5_i == '1', 5, A11_i)][
#'   , A11_i := ifelse(A11_6_i == '1', 6, A11_i)][
#'   , A11_i := ifelse(A11_7_i == '1', 7, A11_i)][
#'   , A11_i := ifelse(A11_8_i == '1', 8, A11_i)][
#'   , CNAE2_AS := Code3toCode2(CNAE_AS)][
#'   , CNAE1_AS := CNAE2toCNAE1(CNAE2_AS)][
#'   , CNAE2_PR := Code3toCode2(CNAE_PR)][
#'   , CNAE1_PR := CNAE2toCNAE1(CNAE2_PR)][
#'   !is.na(CNAE1_AS) & !is.na(CNO_AS)]
#' setnames(preFD_AS.dt, UnitToIDDDNames(names(preFD_AS.dt), preDD))
#' preFD_AS.StQ <- melt_StQ(preFD_AS.dt, preDD)
#'
#' preFF.StQ <- RepoReadWrite::ReadRepoFile(file.path(path, 'E54009/E54009.FF_V1.AA2011.D_1'), 
#' preDD, perl = TRUE)
#' preFF.dt <- dcast_StQ(preFF.StQ)
#' setnames(preFF.dt, IDDDToUnitNames(names(preFF.dt), preDD))
#' preFF_AS.dt <- preFF.dt[
#'   , CNO_PR := ifelse(!is.na(B16_2), B16_2, NA)][
#'   , CNO_PR := ifelse(!is.na(B20a_2), B20a_2, CNO_PR)][
#'   , CNO_PR := ifelse(!is.na(B20m_2), B20m_2, CNO_PR)][
#'   , CNO_AS := ifelse(!is.na(F8_2), F8_2, NA)][
#'   , CNO_AS := ifelse(!is.na(F17a_2), F17a_2, CNO_AS)][
#'   , CNO_AS := ifelse(!is.na(F17m_2), F17m_2, CNO_AS)][
#'   , CNAE_PR := ifelse(!is.na(B15_2), B15_2, NA)][
#'   , CNAE_PR := ifelse(!is.na(B19a_2), B19a_2, CNAE_PR)][
#'   , CNAE_PR := ifelse(!is.na(B19m_2), B19m_2, CNAE_PR)][
#'   , CNAE_AS := ifelse(!is.na(F7_2), F7_2, NA)][
#'   , CNAE_AS := ifelse(!is.na(F16a_2), F16a_2, CNAE_AS)][
#'   , CNAE_AS := ifelse(!is.na(F16m_2), F16m_2, CNAE_AS)][
#'   , SitProf := ifelse(!is.na(B21a), B21a, NA)][
#'   , SitProf := ifelse(!is.na(B21m), B21m, SitProf)][
#'   , SitProf := ifelse(!is.na(B17), B17, SitProf)][
#'   , A11_i := ifelse(A11_1_i == '1', 1, NA)][
#'   , A11_i := ifelse(A11_2_i == '1', 2, A11_i)][
#'   , A11_i := ifelse(A11_3_i == '1', 3, A11_i)][
#'   , A11_i := ifelse(A11_4_i == '1', 4, A11_i)][
#'   , A11_i := ifelse(A11_5_i == '1', 5, A11_i)][
#'   , A11_i := ifelse(A11_6_i == '1', 6, A11_i)][
#'   , A11_i := ifelse(A11_7_i == '1', 7, A11_i)][
#'   , A11_i := ifelse(A11_8_i == '1', 8, A11_i)][
#'   , CNAE2_AS := Code3toCode2(CNAE_AS)][
#'   , CNAE1_AS := CNAE2toCNAE1(CNAE2_AS)][
#'   , CNAE2_PR := Code3toCode2(CNAE_PR)][
#'   , CNAE1_PR := CNAE2toCNAE1(CNAE2_PR)][
#'   !is.na(CNAE1_AS) & !is.na(CNO_AS)]
#' setnames(preFF_AS.dt, UnitToIDDDNames(names(preFF_AS.dt), preDD))
#' preFF_AS.StQ <- melt_StQ(preFF_AS.dt, preDD)
#'
#' fitPar <- new(Class = 'fitParam', edData = preFF_AS.StQ, rawData = preFD_AS.StQ,
#'               selection = FALSE,
#'               formula = 'Ocupacion_35._2.1.5.1._11.1.3._ ~ ActivEcono_35._2.1.5.1._2.1.1._',
#'               selParam = list(maxit = 60))
#' DD <- RepoReadWrite::RepoXLSToDD(file.path(path, 'E54009/E54009.NombresVariables_V2.xlsx'))
#' FD.StQ <- RepoReadWrite::ReadRepoFile(file.path(path, 'E54009/E54009.FD_V2.AA2017.P_1'), DD,
#'  perl = TRUE)
#' FD.dt <- dcast_StQ(FD.StQ)
#' setnames(FD.dt, IDDDToUnitNames(names(FD.dt), DD))
#' FD_AS.dt <- FD.dt[
#'   , CNO_PR := ifelse(!is.na(B16_2), B16_2, NA)][
#'   , CNO_PR := ifelse(!is.na(B20a_2), B20a_2, CNO_PR)][
#'   , CNO_PR := ifelse(!is.na(B20b_2), B20b_2, CNO_PR)][
#'   , CNO_AS := ifelse(!is.na(F9_2), F9_2, NA)][
#'   , CNO_AS := ifelse(!is.na(F19a_2), F19a_2, CNO_AS)][
#'   , CNO_AS := ifelse(!is.na(F19b_2), F19b_2, CNO_AS)][
#'   , CNAE_PR := ifelse(!is.na(B15_2), B15_2, NA)][
#'   , CNAE_PR := ifelse(!is.na(B19a_2), B19a_2, CNAE_PR)][
#'   , CNAE_PR := ifelse(!is.na(B19b_2), B19b_2, CNAE_PR)][
#'   , CNAE_AS := ifelse(!is.na(F8_2), F8_2, NA)][
#'   , CNAE_AS := ifelse(!is.na(F18a_2), F18a_2, CNAE_AS)][
#'   , CNAE_AS := ifelse(!is.na(F18b_2), F18b_2, CNAE_AS)][
#'   , SitProf := ifelse(!is.na(B21a), B21a, NA)][
#'   , SitProf := ifelse(!is.na(B21b), B21b, SitProf)][
#'   , SitProf := ifelse(!is.na(B17), B17, SitProf)][
#'   , CNAE2_AS := Code3toCode2(CNAE_AS)][
#'   , CNAE1_AS := CNAE2toCNAE1(CNAE2_AS)][
#'   !is.na(CNO_AS) & !is.na(CNAE1_AS)]
#' setnames(FD_AS.dt, UnitToIDDDNames(names(FD_AS.dt), DD))
#' set.seed(1)
#' sel <- sample(1:dim(FD_AS.dt)[1], 2000)
#' FD_AS.StQ <- melt_StQ(FD_AS.dt[sel], DD)
#' ObsPredPar <- new(Class = 'categObsPredModelParam',
#'                   Data = FD_AS.StQ,
#'                   VarRoles = list(Units = 'IDHogar', Domains = 'GeoLoc_35._4._2.1.5._1.2.3.'))
#' computeVal(ObsPredPar, fitPar)
#'  # computeVal calls computeEdEfficiency calls (computeRunningEstim and effInd)
#' }
#'
#' @include categObsPredModelParam-class.R fitParam-class.R getSelection.R getEdData.R getRawData.R 
#' getFormula.R setModelFits.R setTrain.R setTest.R computeEdEfficiency.R 
#' Models.R computeProbs.R editPriority.R computeRunningEstim.R RFfit.R rocCurve.R
#'
#' @import data.table StQ
#'
#' @export
setGeneric("computeVal", function(object, fitParam, na.as.category){standardGeneric("computeVal")})

#' @rdname computeVal
#'
#' @export
setMethod(
  f = "computeVal",
  signature = c("categObsPredModelParam", "fitParam", "logical"),
  function(object, fitParam, na.as.category = FALSE){
    
    '..vars' <- '..tgtVars' <- NULL
    
    DD <- getDD(object@Data)
    dataVals <- fitParam@valParam$dataVal
    
    regressors <- getRegressors(object)  
    targetVars <- getRegressands(object)
    id.vars <- object@VarRoles$Units  
    designWeight <- object@VarRoles$DesignW
    vars <- c(id.vars,targetVars,regressors)
    edEffIndicator <- fitParam@valParam$edEffInd
    globalInd <- fitParam@valParam$globalInd
    priorBin <- fitParam@valParam$priorBin
    suffix <- '_ed'
   
    # Para muestra de entrenamiento 
    
    # if (dataVal == 'Test'){
    #     data.dt <- object@Test$data
    #  } else if (dataVal == 'Train'){
    #    data.dt <- object@Train$data
    #  }
  
    # Se realiza bucle para todas las tablas de dataVal y se ejecuta
    # Se realiza bucle para todas las tablas de dataVal y se ejecuta
    for (k in seq(along = dataVals)){
      
        dataVal <- dataVals[k]
        data.dt <-eval(parse(text=paste0('object@',dataVal,'$data')))

        DT <- data.dt[,..vars]
        data.StQ <- melt_StQ(DT, DD)
        object@Data <- data.StQ
        
        ### Cálculo de probabilidades y ordenación de las unidades ###
        object <- computeProbs(object, na.as.category)
        
        ### Validación del modelo y cálculo de la eficiencia
        
        # Dataset con las probabilidades de error
        dataProbs <- object@probs
        
        # Dataset sin depurar con las probabilidades de error y las prioridades  
        edData.StQ <- getEdData(fitParam)
        IDQuals <- getIDQual(edData.StQ, 'MicroData')
        
        edData.dt <- dcast_StQ(edData.StQ, UnitNames = FALSE)
        edData.dt <- edData.dt[, c(IDQuals, targetVars), with = FALSE]
        setnames(edData.dt, targetVars, paste0(targetVars, '_ed'))
        
        data <- merge(dataProbs, edData.dt, by = IDQuals, all.x = TRUE)
        
        if(na.as.category){
          # Valores NA se pasan a *
          for (j in seq_len(ncol(data))){
            set(data, which(is.na(data[[j]])), j, '*')
          }
        }
        
        eff <- lapply(seq(along = targetVars) , function(i){
       
            priorityNames <- grep(paste0("(^priority)([a-z]?)",i), names(data), value = TRUE) 
            
            targetValue <- targetVars[i]
            
            lapply(priorityNames, function(varPriority){
         
                 computeEdEfficiency(data = data,
                                     targetValue = targetValue,
                                     id.vars = IDQuals,
                                     varPriority = varPriority,
                                     designWeight = designWeight,
                                     edEffIndicator = edEffIndicator, 
                                     globalIndicator = globalInd,
                                     priorBin = priorBin,
                                     suffix = suffix) 
          
            })
            
            
        })
        
        names(eff) <- targetVars
        
        for (i  in 1:length(targetVars)){
          priorityNames <- grep(paste0("(^priority)([a-z]?)",i), names(data), value = TRUE) 
          names(eff[[i]]) <- priorityNames
          
        }
        
        # Curva ROC
       
        targets <- list()
        for (j in 1:length(targetVars)){
          targets <- unlist(c(targets,paste0('target',j)))
        }
        
        probs = c('prob_error','prob_errorm')
        tgtVars <- c(id.vars, targets)
        data <- data.dt[, ..tgtVars]
        results <- merge(dataProbs, data, by = id.vars, all.x = TRUE)
        
        
        roc <- rocCurve(data=results, 
                             targets = targets, 
                             targetVars = targetVars,
                             probs = probs)
        
        validation <- list(data = data.dt,
                           randSelIndicators = eff,
                           roc = roc,
                           probs = results)
print(dataVal)        
        if (dataVal == 'Test'){
          
          setTest(object) <- validation
        
        } else if (dataVal == 'Train'){
          
          setTrain(object) <- validation
        
        }
    }    
      
    return(object)
    
  }
)

