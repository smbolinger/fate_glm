
childd <- TRUE
predictors <- c("nest_age", "obs_int", "fdate") # numeric predictors to be used
prList     <- as.list(predictors)
prLabs <-  c("Nest Age (Days)",  "Final Interval (Days)", "Day of Season")
prNames <- c("NEST_AGE", "FINAL_INTERVAL", "END_DATE")
pred2   <- c("species", "cam_fate")
# names(predictors) <- prLabs
names(prList) <- prNames
# now = format(Sys.time(), "_%m%d_%H%M_")
out_dir <- paste0("out",format(Sys.time(), "_%m%d_%H%M"))

# filename <- paste0(homeDir,"glm_script/import_data_", now, ".csv")
# cat("import file name:", filename)
# cat(">> homeDir =", homeDir, "     >> now =", now,"\n\n>> numeric predictor variables:\n\n")
cat(">> homeDir =", homeDir, "     >> out dir =", out_dir,"\n\n>> numeric predictor variables:\n\n")
# print(prList)
cat(paste(names(prList), prList, sep=": ", collapse="    "))

# change CI comma to a dash 
# from https://stackoverflow.com/questions/75637034/insert-dash-in-confidence-interval-instead-of-comma-in-r-gtsummary
my_theme <- list( "pkgwide-str:ci.sep" = " - " )

set_gtsummary_theme(my_theme)

sites_sel <- c("RUTE", "RUTW")
spp_sel <- c("LETE", "CONI")
cam_str <- c("Y")
  
# debug = ifelse(deb, TRUE, FALSE)

## Imputation Simulation settings:
# mod4sim <- modList[1]
# # nrun <- 500
# var_list <-  c("nest_age", "cam_fateD", "cam_fateA", "cam_fateF", "cam_fateHu", "cam_fateS", "speciesLETE", "speciesLETE:nest_age")
# bias_names <- c("bias", "pctBias", "covRate", "avgWidth", "RMSE")
# # met_list <- c("pmm", "rf", "cart")
# met_list <- c("default","pmm", "rf", "cart", "caliber","cc")
# # met_list <- c("default", "rf","cc")
# # resp is passed from fate_GLM1

# if(sett=="default"){
#   add_vhf = FALSE
# } else if (sett=="output"){
#   add_vhf = TRUE
# }


# if(plt){
if(params$plots2){
  corrplots2   = TRUE
  hist2        = TRUE
  tab2         = TRUE
} else {
  corrplots2 = FALSE
  hist2      = FALSE
  tab2       = FALSE
}

if(params$plots1){
  corrplots1   = TRUE
  hist1        = TRUE
  tab1         = TRUE
} else {
  corrplots1   = FALSE
  hist1        = FALSE
  tab1         = FALSE
}

plsettings1 <- c(corrplots1,  
                   hist1,
                   tab1
                   )

plsettings2 <- c(corrplots2,  
                   hist2,
                   tab2
                   )

feb          = FALSE
addFieldFate = FALSE
grouped      = TRUE
compare      = FALSE
old_data     = FALSE
test_UH_fates = FALSE
UHisH = FALSE
UFisF = FALSE