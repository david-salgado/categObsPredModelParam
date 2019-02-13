#' @title Return \linkS4class{data.table} with model fits for prior probabilities
#'
#' @description \code{fitModels} computes ...
#'
#' @param object Object of class \linkS4class{categObsPredModelParam}.
#'
#' @param fitParam Object of class \linkS4class{fitParam}.
#'
#' @return Object of class \linkS4class{categObsPredModelParam} with the slot \code{modelFits}
#' computed with the fitted models.
#'
#' @examples
#'
#' \dontrun{
#' path <- 'R:/USIE/Proyecto_DepSel_VarQual'
#' preDD <- RepoReadWrite::RepoXLSToDD(file.path(path, 'E54009/E54009.NombresVariables_V1.xlsx'))
#' preFD.StQ <- RepoReadWrite::ReadRepoFile(file.path(path, 'E54009/E54009.FD_V1.AA2011.P_1'), preDD, perl = TRUE)
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
#' preFF.StQ <- RepoReadWrite::ReadRepoFile(file.path(path, 'E54009/E54009.FF_V1.AA2011.D_1'), preDD, perl = TRUE)
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
#' FD.StQ <- RepoReadWrite::ReadRepoFile(file.path(path, 'E54009/E54009.FD_V2.AA2017.P_1'), DD, perl = TRUE)
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
#' fitModels(ObsPredPar, fitPar)
#' }
#'
#' @include categObsPredModelParam-class.R fitParam-class.R getSelection.R getEdData.R getRawData.R getFormula.R regfit2.R setModelFits.R
#'
#' @import data.table StQ
#'
#' @export
setGeneric("fitModels", function(object, fitParam){standardGeneric("fitModels")})

#' @rdname fitModels
#'
#' @export
setMethod(
  f = "fitModels",
  signature = c("categObsPredModelParam", "fitParam"),
  function(object, fitParam){

  selection <- getSelection(fitParam)
  formula <- getFormula(fitParam)
  targetVars <- all.vars(formula)

  edData.StQ <- getEdData(fitParam)
  IDQuals_ed <- getIDQual(edData.StQ, 'MicroData')
  targetVars_ed <- c(IDQuals_ed, targetVars)
  edData.dt <- dcast_StQ(edData.StQ, ExtractNames(targetVars))[
      , ..targetVars_ed][
      !is.na(get(targetVars_ed))]
  setnames(edData.dt, targetVars, paste0(targetVars, '_ed'))

  rawData.StQ <- getRawData(fitParam)
  IDQuals_raw <- getIDQual(rawData.StQ, 'MicroData')
  targetVars_raw <- c(IDQuals_raw, targetVars)
  rawData.dt <- dcast_StQ(rawData.StQ, ExtractNames(targetVars))[
      , ..targetVars_raw][
      !is.na(get(targetVars_ed))]
  setnames(rawData.dt, IDQuals_raw, IDQuals_ed)

  trainDT <- merge(edData.dt, rawData.dt, by = IDQuals_ed)
  trainDT <- na.omit(trainDT, cols = c(targetVars, paste0(targetVars, '_ed')))

  maxit <- ifelse(is.null(fitParam@selParam$maxit), 50, fitParam@selParam$maxit)

  if (selection == FALSE) {

    cat('Fitting models for marginal probabilities...')
    models_pi <- regfit2(formula = formula, data = trainDT,
                         id.vars= IDQuals_ed,
                         maxit = maxit, suffix = '_ed', cond = '')
    setnames(models_pi, 'fit' ,'fit.pi')
    cat(' ok.\n')

    cat('Fitting models for probabilities conditional upon value 1...')
    models_cond1 <- regfit2(formula = formula, data = trainDT,
                             id.vars= IDQuals_ed,
                             maxit = maxit, suffix = '_ed', cond = 1)
    setnames(models_cond1, 'fit', 'fit.1')
    cat(' ok.\n')

    cat('Fitting models for probabilities conditional upon value 0...')
    models_cond0 <- regfit2(formula = formula, data = trainDT,
                             id.vars= IDQuals_ed,
                             maxit = maxit, suffix = '_ed', cond = 0)
    setnames(models_cond0, 'fit', 'fit.0')
    cat(' ok.\n')

    models <- models_pi[models_cond1, on = targetVars[1]][models_cond0, on = targetVars[1]]

    setModelFits(object) <- models

    return(object)

  }

  if (selection == TRUE) {



  }

  }
)
