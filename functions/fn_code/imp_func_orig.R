
#`############################################################################
impute <- function(ndGLM1){
  impDat1 <- ndGLM1 %>% # ndGLM1 has only nest_age NAs (all others removed)
    dplyr::select(
      .data$nest,
      .data$nest_age,
      .data$obs_int,
      .data$cam_fate,
      .data$fdate,
      .data$species,
      .data$HF_mis,
      .data$is_u)
  #impDat  <- mice(impDat1,seed=613, m=3, method="pmm")
  impDat  <- mice::mice(impDat1,seed=613, m=20,method="pmm", printFlag=FALSE)
  mice::complete(impDat)
  print(plot(impDat))
  print(attributes(impDat))

  impDat40 <- mice::mice.mids(impDat, maxit=35, print=F)
  print(plot(impDat40))

  return(impDat)
}

###########################################################################
#' Fit models with imputed data
#####################################################################################
runImpMods <- function(
    impDat1,
    m=100,
    method="pmm",
    modList,
    fam=binomial,
    regMethod="glm.fit",
    iter=500,
    resp,
    impute = TRUE,
    pool = FALSE,
    fit = TRUE,
    howMany=FALSE
    ){

  if (howMany) {
    m = 20
    pool = FALSE
  }
  if(impute | howMany){
    impDat  <- mice::mice(impDat1, seed=613, m=m, method=method, printFlag=FALSE)
    mice::complete(impDat)
    print(plot(impDat))
    print(attributes(impDat))
  } else {
    impDat = impDat1 # if you don't need to impute, the data is already in the proper format
  }

  impMod <- vector(mode="list", length=length(modList))
  if(regMethod == "brglm2") {

    regMet = brglm2::brglm_fit

    for (m in seq_along(modList)){
      impMod[[m]] = with(impDat, glm(as.formula(
        # paste0(resp, modList[[m]])), family=fam, method=brglm2::brglm_fit))
        paste0(resp, modList[[m]])), family=fam, method = regMet,
        control=brglmControl(maxit=iter)
        ))
  }
    cat("using brglmFit method")
  } else if(regMethod == "jeffreys") {
    regMet = brglm2::brglm_fit
    type   = "MPL_Jeffreys"
    cat("using Jeffreys's prior")
    for (m in seq_along(modList)){
      impMod[[m]] = with(impDat, glm(as.formula(
        # paste0(resp, modList[[m]])), family=fam, method=brglm2::brglm_fit))
        paste0(resp, modList[[m]])), family=fam, method = regMet, type=type,
        control=brglmControl(maxit=iter)
        ))
      # num[[m]] = how_many_imputations(impMod[[m]], cv=0.05, alpha=0.05)
  }
  } else if (regMethod == "glm.fit") {
    regMet = "glm.fit"
    for (m in seq_along(modList)){
      impMod[[m]] = with(impDat, glm(as.formula(
        # paste0(resp, modList[[m]])), family=fam, method=brglm2::brglm_fit))
        paste0(resp, modList[[m]])), family=fam, method = regMet
        ))
    }
  }
  if(howMany) {
    # num = howManyImputations::how_many_imputations()

    # impDat  <- mice::mice(impDat1, seed=613, m=20, method=method, printFlag=FALSE)
    num = list()
    for(m in seq_along(modList)){
      num[[m]] = how_many_imputations(impMod[[m]], cv=0.05, alpha=0.05)
    }
    # cat("optimal number of imputations (von Hippel 2020): ", num)
  }
  if(pool){
    saveRDS(impMod, "impMod.rds")

    impModPool <- vector(mode="list", length=length(modList))

    for(m in seq_along(modList)){
      impModPool[[m]] = mice::pool(impMod[[m]])
      summary(impModPool[[m]])
      # impModPool[[m]] %>%
      #   gt::gt() # was this new? not working 12 aug 25
    }
    cat("returning pooled data sets")

    return(impModPool)
  } else if(howMany){
    cat("returning optimal number of imputations")
    return(num)
  } else if(fit) {
    cat("returning the fitted model results")
    return(impMod)
  } else if(impute) {
     cat(sprintf("returning all %s imputed data sets for each model", m))
    return(impDat)
  } else {
    cat("make sure only one of howMany and impute is set to TRUE")
    # cat("returning the fitted model results")
  }
}

#`############################################################################
impModTab <- function(impModPool, modList, fileSuffix=""){


  for(m in seq_along(impModPool)){
    imp_mod_tab = impModPool[[m]]$pooled

    fname = imp_mod_tab %>%
      gt::gt() %>%
      gt::fmt_number(decimals = 2) %>%
      # gt::cols_label() %>%
      nestGLM::save_tab(rtf=TRUE)
  }


}
#####################################################################################
#' Extract AIC values and calculate Akaike weights from pooled results
#####################################################################################
aicImpMods <- function(
    impModPool, modList, mods, dir, filePrefix=""
    ){
  aicImpMod <- list()
  # dfImpMod  <- list()
  # impModPool <- impRes
  # function to extract the mean of all 100 values in vals that match the pattern "x"
  mVal <- function(x) mean(as.numeric(vals[grep(x, names(vals))]))
  # mVal("glanced.AIC")
  extrNames <- c("glanced.AIC",           # names to extract
                 "glanced.deviance",
                 "glanced.df.residual",
                 "glanced.logLik")
  for(m in seq_along(impModPool)){
    vals = unlist(x=impModPool[[m]])[-1] # also remove the first element, which is a list
    # names(vals)
    # aic = vals$glanced.AIC
    # aicImpMod[[m]] = mean(vals[grep("glanced.AIC", names(vals))])
    # vals[grep("glanced.AIC", names(vals))]
    aicImpMod[[m]] = lapply(extrNames, mVal)
    names(aicImpMod[[m]]) <- extrNames
  }
  # akaikeWts <- MuMIn::Weights(unlist(aicImpMod)) # this isn't correct anymore
  saveRDS(aicImpMod, file = "aicImpMod.rds")
  aicImpMod <- unlist(aicImpMod) # unlist so you can go through the values easily
  # impAICtab <- data.frame(modname    = modList,
  impAICtab <- data.frame(num = c(1:length(mods)),
                          modname    = modnames(mods),
                          dfResid    = as.integer(aicImpMod[grep(extrNames[3], names(aicImpMod))]),
                          logLik     = aicImpMod[grep(extrNames[4], names(aicImpMod))],
                          aic        = aicImpMod[grep(extrNames[1], names(aicImpMod))],
                          deltaAIC   = numeric(length=length(modList)),
                          akaikeWt   = numeric(length=length(modList)),
                          rdev       = aicImpMod[grep(extrNames[2], names(aicImpMod))]
                          )

  # impAICtab <- data.frame(modName=modList, aic=unlist(aicImpMod), aicWt=akaikeWts)
  impAICtab$akaikeWt <- MuMIn::Weights(impAICtab$aic)
  # impAICtab$
  impAICtab <- impAICtab[order(impAICtab$aic),] # order isn't working anymore?
  # impAICtab$deltaAIC <- c(0, diff(impAICtab$aic)) $ NOT RIGHT - calculates difference from previous value, not min
  impAICtab$deltaAIC <- c(impAICtab$aic - min(impAICtab$aic))
  # impAICtab$k        <- length(rowMeans(coef(impModPoo)))
  saveRDS(impAICtab, file="impAICtab.rds")
  # deci <- c(0,0,0,2,2,2,2,2) # I don't totally understand the cols_add() approach on the gt site
  impAIC <- gt::gt(impAICtab)  %>%
    gt::fmt_number(columns=c( logLik, aic, deltaAIC, akaikeWt),
                   decimals=2) %>%
    gt::cols_label(
    # cols_label( # why does this not have gt:: in front of it?
      modname  = "Model",
      dfResid  = "Degrees of\nFreedom",
      logLik   = "Log\nLikelihood",
      aic      = "AICc",
      deltaAIC = "delta AIC",
      akaikeWt = "Akaike Weight",
      rdev     = "Residual\nDeviance"
      )
  filename <- impAIC %>% nestGLM::save_tab(dir = dir, suffix = filePrefix, rtf = TRUE)
  return(filename)
}

