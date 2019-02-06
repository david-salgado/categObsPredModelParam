 path <- 'R:/USIE/Proyecto_DepSel_VarQual'
 preDD <- RepoReadWrite::RepoXLSToDD(file.path(path, 'E54009/E54009.NombresVariables_V1.xlsx'))
 preFD.StQ <- RepoReadWrite::ReadRepoFile(file.path(path, 'E54009/E54009.FD_V1.AA2011.P_1'), preDD, perl = TRUE)
 preFD.dt <- dcast_StQ(preFD.StQ)
 setnames(preFD.dt, IDDDToUnitNames(names(preFD.dt), preDD))
 preFD_AS.dt <- preFD.dt[
   , CNO_PR := ifelse(!is.na(B16_2), B16_2, NA)][
   , CNO_PR := ifelse(!is.na(B20a_2), B20a_2, CNO_PR)][
   , CNO_PR := ifelse(!is.na(B20m_2), B20m_2, CNO_PR)][
   , CNO_AS := ifelse(!is.na(F8_2), F8_2, NA)][
   , CNO_AS := ifelse(!is.na(F17a_2), F17a_2, CNO_AS)][
   , CNO_AS := ifelse(!is.na(F17m_2), F17m_2, CNO_AS)][
   , CNAE_PR := ifelse(!is.na(B15_2), B15_2, NA)][
   , CNAE_PR := ifelse(!is.na(B19a_2), B19a_2, CNAE_PR)][
   , CNAE_PR := ifelse(!is.na(B19m_2), B19m_2, CNAE_PR)][
   , CNAE_AS := ifelse(!is.na(F7_2), F7_2, NA)][
   , CNAE_AS := ifelse(!is.na(F16a_2), F16a_2, CNAE_AS)][
   , CNAE_AS := ifelse(!is.na(F16m_2), F16m_2, CNAE_AS)][
   , SitProf := ifelse(!is.na(B21a), B21a, NA)][
   , SitProf := ifelse(!is.na(B21m), B21m, SitProf)][
   , SitProf := ifelse(!is.na(B17), B17, SitProf)][
   , A11_i := ifelse(A11_1_i == '1', 1, NA)][
   , A11_i := ifelse(A11_2_i == '1', 2, A11_i)][
   , A11_i := ifelse(A11_3_i == '1', 3, A11_i)][
   , A11_i := ifelse(A11_4_i == '1', 4, A11_i)][
   , A11_i := ifelse(A11_5_i == '1', 5, A11_i)][
   , A11_i := ifelse(A11_6_i == '1', 6, A11_i)][
   , A11_i := ifelse(A11_7_i == '1', 7, A11_i)][
   , A11_i := ifelse(A11_8_i == '1', 8, A11_i)][
   , CNAE2_AS := Code3toCode2(CNAE_AS)][
   , CNAE1_AS := CNAE2toCNAE1(CNAE2_AS)][
   , CNAE2_PR := Code3toCode2(CNAE_PR)][
   , CNAE1_PR := CNAE2toCNAE1(CNAE2_PR)][
   !is.na(CNAE1_AS) & !is.na(CNO_AS)]
 setnames(preFD_AS.dt, UnitToIDDDNames(names(preFD_AS.dt), preDD))
 preFD_AS.StQ <- melt_StQ(preFD_AS.dt, preDD)

 preFF.StQ <- RepoReadWrite::ReadRepoFile(file.path(path, 'E54009/E54009.FF_V1.AA2011.D_1'), preDD, perl = TRUE)
 preFF.dt <- dcast_StQ(preFF.StQ)
 setnames(preFF.dt, IDDDToUnitNames(names(preFF.dt), preDD))
 preFF_AS.dt <- preFF.dt[
   , CNO_PR := ifelse(!is.na(B16_2), B16_2, NA)][
   , CNO_PR := ifelse(!is.na(B20a_2), B20a_2, CNO_PR)][
   , CNO_PR := ifelse(!is.na(B20m_2), B20m_2, CNO_PR)][
   , CNO_AS := ifelse(!is.na(F8_2), F8_2, NA)][
   , CNO_AS := ifelse(!is.na(F17a_2), F17a_2, CNO_AS)][
   , CNO_AS := ifelse(!is.na(F17m_2), F17m_2, CNO_AS)][
   , CNAE_PR := ifelse(!is.na(B15_2), B15_2, NA)][
   , CNAE_PR := ifelse(!is.na(B19a_2), B19a_2, CNAE_PR)][
   , CNAE_PR := ifelse(!is.na(B19m_2), B19m_2, CNAE_PR)][
   , CNAE_AS := ifelse(!is.na(F7_2), F7_2, NA)][
   , CNAE_AS := ifelse(!is.na(F16a_2), F16a_2, CNAE_AS)][
   , CNAE_AS := ifelse(!is.na(F16m_2), F16m_2, CNAE_AS)][
   , SitProf := ifelse(!is.na(B21a), B21a, NA)][
   , SitProf := ifelse(!is.na(B21m), B21m, SitProf)][
   , SitProf := ifelse(!is.na(B17), B17, SitProf)][
   , A11_i := ifelse(A11_1_i == '1', 1, NA)][
   , A11_i := ifelse(A11_2_i == '1', 2, A11_i)][
   , A11_i := ifelse(A11_3_i == '1', 3, A11_i)][
   , A11_i := ifelse(A11_4_i == '1', 4, A11_i)][
   , A11_i := ifelse(A11_5_i == '1', 5, A11_i)][
   , A11_i := ifelse(A11_6_i == '1', 6, A11_i)][
   , A11_i := ifelse(A11_7_i == '1', 7, A11_i)][
   , A11_i := ifelse(A11_8_i == '1', 8, A11_i)][
   , CNAE2_AS := Code3toCode2(CNAE_AS)][
   , CNAE1_AS := CNAE2toCNAE1(CNAE2_AS)][
   , CNAE2_PR := Code3toCode2(CNAE_PR)][
   , CNAE1_PR := CNAE2toCNAE1(CNAE2_PR)][
   !is.na(CNAE1_AS) & !is.na(CNO_AS)]
 setnames(preFF_AS.dt, UnitToIDDDNames(names(preFF_AS.dt), preDD))
 preFF_AS.StQ <- melt_StQ(preFF_AS.dt, preDD)

 fitPar <- new(Class = 'fitParam', edData = preFF_AS.StQ, rawData = preFD_AS.StQ,
               selection = FALSE,
               formula = 'Ocupacion_35._2.1.5.1._11.1.3._ ~ ActivEcono_35._2.1.5.1._2.1.1._',
               selParam = list(maxit = 60))


 DD <- RepoReadWrite::RepoXLSToDD(file.path(path, 'E54009/E54009.NombresVariables_V2.xlsx'))
 FD.StQ <- RepoReadWrite::ReadRepoFile(file.path(path, 'E54009/E54009.FD_V2.AA2017.P_1'), DD, perl = TRUE)
 FD.dt <- dcast_StQ(FD.StQ)
 setnames(FD.dt, IDDDToUnitNames(names(FD.dt), DD))
 FD_AS.dt <- FD.dt[
   , CNO_PR := ifelse(!is.na(B16_2), B16_2, NA)][
   , CNO_PR := ifelse(!is.na(B20a_2), B20a_2, CNO_PR)][
   , CNO_PR := ifelse(!is.na(B20b_2), B20b_2, CNO_PR)][
   , CNO_AS := ifelse(!is.na(F9_2), F9_2, NA)][
   , CNO_AS := ifelse(!is.na(F19a_2), F19a_2, CNO_AS)][
   , CNO_AS := ifelse(!is.na(F19b_2), F19b_2, CNO_AS)][
   , CNAE_PR := ifelse(!is.na(B15_2), B15_2, NA)][
   , CNAE_PR := ifelse(!is.na(B19a_2), B19a_2, CNAE_PR)][
   , CNAE_PR := ifelse(!is.na(B19b_2), B19b_2, CNAE_PR)][
   , CNAE_AS := ifelse(!is.na(F8_2), F8_2, NA)][
   , CNAE_AS := ifelse(!is.na(F18a_2), F18a_2, CNAE_AS)][
   , CNAE_AS := ifelse(!is.na(F18b_2), F18b_2, CNAE_AS)][
   , SitProf := ifelse(!is.na(B21a), B21a, NA)][
   , SitProf := ifelse(!is.na(B21b), B21b, SitProf)][
   , SitProf := ifelse(!is.na(B17), B17, SitProf)][
   , CNAE2_AS := Code3toCode2(CNAE_AS)][
   , CNAE1_AS := CNAE2toCNAE1(CNAE2_AS)][
   !is.na(CNO_AS) & !is.na(CNAE1_AS)]
 setnames(FD_AS.dt, UnitToIDDDNames(names(FD_AS.dt), DD))
 set.seed(1)
 sel <- sample(1:dim(FD_AS.dt)[1], 2000)
 FD_AS.StQ <- melt_StQ(FD_AS.dt[sel], DD)
 ObsPredPar <- new(Class = 'categObsPredModelParam',
                   Data = FD_AS.StQ,
                   VarRoles = list(Units = c('IDHogar', 'NOrden'),
                                   Domains = character(0),
                                   DesignW = 'Parametro_07._6.1._2.1.5.1.',
                                   Regressands = 'Ocupacion_35._2.1.5.1._11.1.3._',
                                   Regressors = 'ActivEcono_35._2.1.5.1._2.1.1._'))
 ObsPredPar <-  fitModels(ObsPredPar, fitPar)

 ObsPredPar <- computeProbs(ObsPredPar)

ImpParam <- new(Class = 'MeanImputationParam',
                VarNames = c('Ocupacion_35._2.1.5.1._11.1.3._'),
                DomainNames =  c('GeoLoc_35._4._2.1.5._1.2.3.'))

AbsLossPar <- new(Class = 'AbsLossErrorMomentParam',
                  VarNames =  c("Ocupacion_35._2.1.5.1._11.1.3._"),
                  Homoskedastic = c(TRUE),
                  Imputation = ImpParam)

ErrorMoments <- ComputeErrorMoment(ObsPredPar, AbsLossPar)
