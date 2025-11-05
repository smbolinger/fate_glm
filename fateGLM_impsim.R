#
# as a .Rmd, this script would stop after the first loop with no error msg when I ran it 
# from the Linux CLI. But it really doesn't need to be a .Rmd?

library(brglm2) # penalized logistic regression
library(CALIBERrfimpute) # could access using ::
library(gt)
library(gtsummary)
library(tidyverse)
source("mice_functions.R")
source("imp_sim_functions.R")
source("missing_data.R")

# spec <- matrix(c(
#   'lin',  'l', 1, "logical",
#   'help', 'h', 0, "logical",
#   'nrun', 'n', 2, "numeric",
#   'hdir', 'r', 2, "character",
#   'deb',  'd', 2, "logical"
# ), byrow=TRUE, ncol=4)
# opt = getopt::getopt(spec = spec)

params <- list(nrun=100, hdir="/home/wodehouse/projects/fate_glm/", deb=FALSE, lin=FALSE, m=20)
# params <- list(nrun=100, deb=FALSE, lin=FALSE)
arg <- commandArgs(trailingOnly=TRUE)
if(length(arg)==0){
  # stop("at minimum, needs argument for 'lin' (T or F)")
  cat("\n\n/////////////////////////////////////////////////////////////////////////////////////////////\n")
  # cat("\n** NOTE ** using default of linux = FALSE, nrun = 3, & debug = FALSE\n\n")
  cat("\n** NOTE ** no arguments provided - using default of linux = FALSE\n\n")
  cat("/////////////////////////////////////////////////////////////////////////////////////////////\n\n\n")
  # print("\n** NOTE ** using default of linux = FALSE, nrun = 5, & debug = FALSE\n\n")
} else if(length(arg) > 0){
  cat("arg =")
  print(arg)
  for(a in arg){
    # if(is.numeric(a)) params$nrun <- a
    if(grepl("^\\d+$", a)) params$nrun  <- a
    else if(a=="lin") params$lin        <- TRUE
    else if(a=="deb") params$deb        <- TRUE
    else if(grepl("m\\d+", a)) params$m <- as.numeric(str_extract(a, "\\d+"))
    # else if()
  }
}
if(params$lin==FALSE){
  params$hdir <- "C:/Users/sarah/Dropbox/Models/fate_glm/" 
  params$nrun <-3
  # suffix <- "5reps"
  # params$deb <- TRUE
}


# nrun <- params$nrun
# debug <- params$debug
# # mod4sim <- modList[1]
# hdir <- params$home_dir

suffix <- sprintf("%sruns", params$nrun)
cat(">> home directory:", params$hdir, "\t> & number of runs:", params$nrun, "\t> & output suffix:", suffix)
modList <- readLines("modList.txt")
mods4sim <- modList[c(1,8,16) ]
# mods4sim
# mods4sim <- modList[c(1,8) ]
names(mods4sim) <- c("m1", "m8", "m16")

# dat4sim <- read.csv("dat_complete_ff8.csv", stringsAsFactors = TRUE)
dat4sim <- read.csv("dat_complete.csv", stringsAsFactors = TRUE)
# make 'H' the reference category:
dat4sim$cam_fate <- relevel(dat4sim$cam_fate, ref="H")
 
# made the responses into "yes/no" so I could import them automatically as factors
prVars <- c("species", "cam_fate", "obs_int", "nest_age", "fdate")
resp_list <- c("is_u", "HF_mis")

## FOR TESTING:
if(FALSE){
  # mod4imp <- modList[1]
  var_list <-  c("nest_age", "cam_fateD", "cam_fateA", "cam_fateF", "cam_fateHu", "cam_fateS", "speciesLETE", "speciesLETE:nest_age", "speciesLETE:obs_int")
  bias_names <- c("bias", "pctBias", "covRate", "avgWidth", "RMSE")
  # met_list <- c("pmm", "rf", "cart")
  # met_list <- c("default","pmm", "rf", "cart", "caliber","cc")
  met_list <- c("default", "rf","cc")
  met_list <- c("default","rf", "caliber", "cc")
  params$nrun <- 2
  params$deb <- TRUE
  # debug <- FALSE
  # resp is passed from fate_GLM1
}

# var_list <-  c("nest_age", "cam_fateD", "cam_fateA", "cam_fateF", "cam_fateHu", "cam_fateS", "speciesLETE", "speciesLETE:nest_age") # just the vars w/ missing data or interactions
var_list <-  c("nest_age", "cam_fateD", "cam_fateA", "cam_fateF", "cam_fateHu", "cam_fateS", "speciesLETE", "speciesLETE:nest_age", "speciesLETE:obs_int", "obs_int", "fdate") # all vars
bias_names <- c("value","bias", "pctBias", "covRate", "avgWidth", "RMSE", "SD")
# bias_names <- c("bias","pctBias", "covRate", "avgWidth", "RMSE", "SD")
# can't actually use the interaction term in the imputation model bc need identical data for AIC comparison
# won't so this because I also don't know  the specific relationship of the interaction
# , met_list <- c("default","pmm", "rf", "cart", "caliber","default.int","pmm.int","passive.int","cc")
# met_list <- c("default","pmm", "rf", "cart", "caliber","cc","full") # don't need full here?
met_list <- c("default","pmm", "rf", "cart", "caliber","cc")# don't need full here?

# col_sel <- c(prVars,resp) # columns to select, as strings
# cat("\n\n>> models to fit:\n", paste(mods4sim, collapse="\n"))
cat("\n\n>> number of imputations:", params$m, class(params$m))
cat("\t>> & methods to test:", met_list)
cat("\n\n>> bias to be calculated:", bias_names, "\n")
cat("\n\n>>>> date & time:", format(Sys.time(), "%d-%b %H:%M\n"))

#########################################################################################
#########################################################################################

sim_dat <- mkSimDat(nd = dat4sim, method = "amp", wt = TRUE, debug = params$deb, convFact = TRUE)
# sim_dat <- mkSimDat(nd = ndGLM_scl_cc, method = "amp", wt = TRUE, debug = debug, convFact = TRUE)
missing_tab("sim_dat",prVars,)

# names(mods4sim) <- c("m1", "m8")
if(FALSE){
  z <- 1
  r <- "is_u"
  # r <- "HF_mis"
  params$deb <- TRUE
  met_list <- c("default", "rf","cc") # can use fewer methods to make it faster
  mods4sim
  for(m in seq_along(mnames)){
    # modnum = str_extract(mnames[m],"\\d+")
    mod    = mods4sim[m]
    fitReal <- glm(as.formula(paste0(r, mod)),
                   # data=ndGLM_scl_cc,
                   data=dat4sim,
                   family=binomial,
                   method=brglm2::brglm_fit)
    # ,
                   # control=brglmControl(maxit=iter) )
    headr <- paste("Regression Summary for Model:",
                   paste(r, mod, sep=" "), 
                   collapse="\n"
    )
                   # modnames(list(x = mods[[modNum]]), null=FALSE), sep="\n")
    tabReg <- gtsummary::tbl_regression(fitReal, exponentiate=TRUE) %>%
      gtsummary::as_gt() %>%
      gt::tab_header(title=headr)
    print(tabReg)
  }
}
# mods4sim
# for(z in seq_along(mods4sim)){
#   # , cat("\n>>model:", names(model), model)
#   if(params$deb){
#     cat("\n\n********************************************************************************************\n")
#     cat("\n********************************************************************************************\n")
#     cat("\n/////////////////////////////////////////////////////////////////////////////////////////////\n")
#   }
#   cat("\n********************************************************************************************")
#   cat("\n>> model:", names(mods4sim)[z], " | ", mods4sim[z])
#   cat("\n********************************************************************************************\n")
#   if(params$deb){
#     cat("\n/////////////////////////////////////////////////////////////////////////////////////////////\n")
#     cat("\n********************************************************************************************\n")
#     cat("\n********************************************************************************************\n")
#   }
for(r in resp_list){
  col_sel <- c(prVars,r) # columns to select, as strings
  # if(params$deb){
  cat("\n\n********************************************************************************************")
  cat("\n>> response:", r,"\n\t& columns for imputation:", col_sel)
  cat("\n********************************************************************************************\n\n")
  # }
  # imp_sim <- runSim(datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = resp, vars = var_list, mod = mod4sim, nruns=nrun, debug = FALSE) # don't want to set seed
  # imp_sim <- runSim(dat=ndGLM_scl_cc, datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = r, vars = var_list, mod = mod4sim, nruns=nrun, debug = debug) # don't want to set seed
  # imp_sim <- runSim(datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = r, vars = var_list, mod = mod4sim, nruns=nrun, debug = debug) # don't want to set seed
  # imp_sim <- runSim(datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = r, vars = var_list, mod = mods4sim[z], nruns=params$nrun, debug = params$deb) # don't want to set seed
  # imp_sim <- runSim(datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = r, vars = var_list, mods = mods4sim, m=15, nruns=params$nrun, debug = params$deb) # don't want to set seed
  imp_sim <- runSim(datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = r, vars = var_list, mods = mods4sim, m=params$m, nruns=params$nrun, debug = params$deb) # don't want to set seed
  # bias_out <- parAvg(fullDat = ndGLM_scl_cc, impDat = imp_sim,resp = r, vars = var_list, mod = mod4sim,mets = met_list, biasVals = bias_names, debug = debug)
  # bias_out <- parAvg(fullDat = ndGLM_scl_cc, impDat = imp_sim,resp = r, vars = var_list, mod = mods4sim[z], mets = met_list, biasVals = bias_names, debug = debug)
  if(params$deb){
    cat("\n\n********************************************************************************************\n")
    cat(">>>>> BIAS VALUES: \n")
    cat("********************************************************************************************\n")
  }
  # bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,resp = r, vars = var_list, mod = mods4sim[z], mets = met_list, biasVals = bias_names, debug = params$deb)
  # bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,resp = r, vars = var_list, modnum = z, mets = met_list, biasVals = bias_names, debug = params$deb)
  bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,hdir = params$hdir,resp = r, vars = var_list, mods=mods4sim, mets = met_list, biasVals = bias_names, debug = params$deb)
  # biasfile <- paste0(params$home_dir, sprintf("out/bias_vals_%s_%s.rds", r, names(mods4sim)[z]))
  # biasfile <- paste0(params$hdir, sprintf("out/bias_vals_%s_%s_%s_.rds", r, names(mods4sim)[z], params$suffix))
  # biasfile <- paste0(params$hdir, sprintf("out/bias_vals_%s_%s_%s_.rds", r, names(mods4sim)[z], suffix))
  # # saveRDS(bias_out, sprintf("out/bias_vals_%s_%s.rds",r, names(mods4sim)[z]))
  # saveRDS(bias_out, biasfile)
  # biasfile1 <- paste0(params$hdir, sprintf("out/bias_vals_%s_%s_%s_.csv", r, names(mods4sim)[z], suffix))
  # # biasfile1
  # # write.csv(bias_out, file = sprintf("out/bias_vals_%s_%s.csv", r, names(mods4sim)[z]))# write to csv in case script aborts 
  # # write.csv(bias_out, file = biasfile1, row.names = FALSE)# write to csv in case script aborts 
  # ## NOTE - NEED ROW NAMES - that's where the param names are stored |> 
  # write.csv(bias_out, file = biasfile1)# write to csv in case script aborts
  # if(params$deb){
  # }
  # print(bias_out) # print the output to console
  # cat("\n******************************************************************************************\n")
}
# }
