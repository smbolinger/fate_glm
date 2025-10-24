
if(sett == "default"){
  # homeDir <- "C:/Users/sarah/Dropbox/nest_models/"
  # homeDir <- "C:/Users/sarah/Dropbox/Models/fate_glm/"
  
  predictors <- c("nest_age", "obs_int", "fdate") # numeric predictors to be used
  prList     <- as.list(predictors)
  prLabs <-  c("Nest Age (Days)",  "Final Interval (Days)", "Day of Season")
  prNames <- c("NEST_AGE", "FINAL_INTERVAL", "END_DATE")
  # names(predictors) <- prLabs
  names(prList) <- prNames
  now = format(Sys.time(), "_%m%d_%H%M_")
  
  # filename <- paste0(homeDir,"glm_script/import_data_", now, ".csv")
  # cat("import file name:", filename)
  cat(">> homeDir =", homeDir, "     >> now =", now,"\n\n>> numeric predictor variables:\n\n")
  # print(prList)
  cat(paste(names(prList), prList, sep=": ", collapse="    "))
  
  # change CI comma to a dash 
  # from https://stackoverflow.com/questions/75637034/insert-dash-in-confidence-interval-instead-of-comma-in-r-gtsummary
  my_theme <- list( "pkgwide-str:ci.sep" = " - " )
  
  set_gtsummary_theme(my_theme)
  
  
  debug        = FALSE
  
  # missing_diag = FALSE
  # missing_diag = TRUE # moved to params
  separation   = FALSE
  
  corrplots1   = FALSE
  corrplots2   = TRUE
  
  hist1        = FALSE
  hist2        = TRUE
  
  tab1         = FALSE
  tab2         = TRUE
  
  plsettings1 <- c(corrplots1,  
                     hist1,
                     tab1
                     )
  
  plsettings2 <- c(corrplots2,  
                     hist2,
                     tab2
                     )
  
  sites_sel <- c("RUTE", "RUTW")
  spp_sel <- c("LETE", "CONI")
  cam_str <- c("Y")
  
  feb          = FALSE
  addFieldFate = FALSE
  grouped      = TRUE
  compare      = FALSE
  old_data     = FALSE
  
  UHisH = TRUE
  # UHisH = FALSE
  # UFisF = TRUE
  UFisF = FALSE
}