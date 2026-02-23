
############## AIC TABLE #################################################
aic_mod <- function(mods){
 modEQ = modnames(mods)
  # modEQ = sapply(mods, function(x) stringr::str_extract(dp(x$formula)))
  nestAIC <- tryCatch(
    {
      AICcmodavg::aictab(cand.set = mods, modnames = modEQ, sort = T) %>%
        dplyr::mutate(df = K-1) %>%
        cat("k=", K, "df=", df) %>%
        cat("; mod names=\n", paste(Modnames, sep="\n")) %>%
        dplyr::select(Modnames, ,AICc, Delta_AICc, df, AICcWt, LL)
        # rename()
    },
    error=function(e){

        cat("using bbmle; mod names=\n", paste(modEQ, sep="\n"))
        bbmle::AICctab(mods, weights=TRUE, logLik=TRUE, base=TRUE, nobs=121, mnames=modEQ) %>%
          as.data.frame() %>%
          dplyr::select(df, AICc, dAICc, weight, logLik, dLogLik)
    }
  )
   AICtab <- nestAIC %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric), ~round(.x, digits=2))) %>%
    gt::gt(rownames_to_stub = TRUE) %>%
    gt::tab_header(title=paste("AIC scores")) %>%
    gt::tab_options(
      column_labels.font.weight = "bold"
    ) %>%
  return(AICtab)
}

#'
############## BIC TABLE #################################################
bic_mod <- function(mods){

  modEQ <- paste(sapply(mods,
                        function(x) stringr::str_extract(dp(x$call), "(?<=~\\s).+(?=,\\sf)")
                        ), sep = " ")

  nestBIC <- AICcmodavg::bictab(cand.set = mods, modnames = modEQ, sort = T) # include VIF?

  BICtab <- gt::gt(nestBIC) %>%
    gt::tab_header(title="BIC scores") %>%
    gt::tab_style(style =
                gt::cell_text(size="small"),
              locations = gt::cells_body())
  return(BICtab)
}

############## REGRESSION TABLE ##########################################
tabRegr <- function(modNum, mods, resp,
                    vars=c('species', 'nest_age', 'obs_int', 'cam_fate', 'fdate'),
                    newNames=c('SPECIES', 'NEST_AGE', 'FINAL_INTERVAL', 'TRUE_FATE', 'END_DATE'),
                    suffix="",
                    debug=F){
  # browser()
  now = format(Sys.time(), "%m%d_%H%M%S") # seconds bc tables all generated in < 1 min
  nm       <- function(x) substitute(x)
  if (resp=="is_u"){
    r = "MARKED_UNKNOWN"
  } else if(resp=="HF_mis" | resp=="misclass"){
    r = "MISCLASSIFIED"
  }
  if(debug) cat("response is", r, "\n\n")
  modEQ <- paste(sapply(
    mods,
    function(x) stringr::str_extract(dp(x$call), "(?<=~\\s)(.*)(?=\")")
    ), sep = " ")
  modVar <- stringr::word(modEQ)# extracts words
  if(debug) cat("extracted mod var:", modVar, "\n\n")
  # vars <- c('species', 'nest_age', 'obs_int', 'cam_fate', 'fdate')
  # newName <- c('SPECIES', 'NEST_AGE', 'FINAL_INTERVAL', 'TRUE_FATE', 'END_DATE')
  varNames <- ifelse(vars %in% modVar, paste(vars,"~'", newNames,"'",sep=""), NA)
  varNames <- varNames[!is.na(varNames)]
  vn <- as.list(varNames) #maybe all of this is not necessary?
  vnames <- as.list(newNames)
  if(debug) print(vnames)
  names(vnames) <- vars
  if(debug) print(vnames)
  tabOR <- gtsummary::tbl_regression(mods[[modNum]],
                          label = vnames,
                          exponentiate=T)
  headr <- paste("Regression Summary for Model:",
                 modnames(list(x = mods[[modNum]]), null=FALSE),
                 sep="\n")


  now = format(Sys.time(), "%m%d_%H%M_")
  filename3  <- sprintf("reg_%s_%s.rtf", now, suffix)
  filename4  <- sprintf("reg_%s_%s.png", now, suffix)
  # tabOR %>% gt() %>% gtsave(filename=filename3, path="analysis/", vwidth=1200, vheight=800)
  tabOR %>%
    gtsummary::as_gt() %>%
    # gt::tab_header(title=paste(question)) %>%
    gt::tab_header(title=headr) %>%
    gt::gtsave(filename=filename3, path="analysis/", vwidth=1200, vheight=800)


  tabOR %>%
    gtsummary::as_gt() %>%
    # gt::tab_header(title=paste(question)) %>%
    gt::tab_header(title=headr) %>%
    gt::gtsave(filename=filename4, path="analysis/", vwidth=1200, vheight=800)

  # return(tabOR)
  return( filename4)
}


############## FILE NAMES ################################################
make_fname <- function(name, suffix, extension, nowDigits=6){
  if(nowDigits==6) {
    now = format(Sys.time(), "_%m%d_%H%M_")
  }
  fname <- paste0(name, suffix, now, extension)
  return(fname)
}
############## SAVE TABLES ###############################################
save_tab <- function(tabName, dpi=(1800/6), dir, suffix="", rtf=FALSE){

  now = format(Sys.time(), "_%m%d_%H%M%S") # seconds bc tables all generated in < 1 min
  nm       <- function(x) substitute(x)
  # dpi      <- (1800/6)                          # img width (px) / desired img width (in)
  if(rtf){
    file_name <- paste0(nm(tab), suffix, now, ".rtf") # name of tab will always be tab within the function?
    gt::gtsave(tab, file_name, path=paste0(dir,"analysis/"))
  }

  file_name1 <- paste0(nm(tab), now, suffix, ".png")
  gt::gtsave(tab, file_name1, path=paste0(dir,"analysis/"))

  # file_name2 <- paste0(nm(tab), now, "_rounded.png")
  # tab2 <- tab %>% fmt_number(decimals=2)
  # gtsave(tab2, file_name2, path-"analysis/")
  return(file_name1)
}

