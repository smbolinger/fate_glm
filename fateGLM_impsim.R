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
source("other_imp.R")

#########################################################################################

params <- list(nrun=100, hdir="/home/wodehouse/projects/fate_glm/",
               deb=FALSE,
               xdeb=FALSE, # for obsessively checking things - not useful for normal debugging & lots of text output
               ipl=FALSE,
               lin=FALSE,
               m=20)
# params <- list(nrun=100, deb=FALSE, lin=FALSE)
arg <- commandArgs(trailingOnly=TRUE)
if(length(arg)==0){
  # stop("at minimum, needs argument for 'lin' (T or F)")
  cat("\n\n/////////////////////////////////////////////////////////////////////////////////////////////\n")
  cat("\n** NOTE ** no arguments provided - using default of linux = FALSE\n\n")
  cat("/////////////////////////////////////////////////////////////////////////////////////////////\n\n\n")
} else if(length(arg) > 0){
  cat("arg =")
  print(arg)
  for(a in arg){
    # if(is.numeric(a)) params$nrun <- a
    if(grepl("^\\d+$", a)) params$nrun  <- a
    else if(a=="lin") params$lin        <- TRUE
    else if(a=="deb") params$deb        <- TRUE
    else if(a=="xdeb") params$xdeb        <- TRUE
    else if(a=="ipl") params$ipl       <- TRUE
    else if(grepl("m\\d+", a)) params$m <- as.numeric(str_extract(a, "\\d+"))
    # else if()
  }
}
if(params$lin==FALSE){
  params$hdir <- "C:/Users/sarah/Dropbox/Models/fate_glm/" 
  params$m <- 5
  params$nrun <-5
  params$deb <- TRUE
  params$xdeb <- TRUE
}

#########################################################################################

debugging <- FALSE # for uickly setting values when working in the file with the functions
suffix <- sprintf("%sruns", params$nrun)
cat(">> home directory:", params$hdir, "\t> & number of runs:", params$nrun, "\t> & output suffix:", suffix)

modList <- readLines("modList.txt")
mods4sim <- modList[c(1,8,16) ]
# mods4sim
names(mods4sim) <- c("m1", "m8", "m16")

dat4sim <- read.csv("dat_complete.csv", stringsAsFactors = TRUE)
# dat4simnum <- read.csv("dat_num.csv"  )
# make 'H' the reference category:
dat4sim$cam_fate <- relevel(dat4sim$cam_fate, ref="H")
dat4sim$species <- relevel(dat4sim$species, ref="LETE")
# dat4simnum$cfate <- relevel(dat4simnum$cfate, ref=1)
levels(dat4sim$HF_mis) <- c(0,1)
levels(dat4sim$is_u)   <- c(0,1)
 
#########################################################################################

# made the responses into "yes/no" so I could import them automatically as factors
prVars <- c("species", "cam_fate", "obs_int", "nest_age", "fdate")
resp_list <- c("is_u", "HF_mis")

var_list <-  c("nest_age", "cam_fateD", "cam_fateA", "cam_fateF", "cam_fateHu", "cam_fateS", "speciesCONI", "speciesCONI:nest_age", "speciesCONI:obs_int", "obs_int", "fdate") # all vars
bias_names <- c("value","bias", "pctBias", "covRate", "avgWidth", "RMSE", "SD")
met_list <- c("default","pmm", "rf", "cart", "caliber","passive", "stratify","cc")# don't need full here?

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
}

if(exists("deb_new")) params$deb <- deb_new 
if(exists("nrun_new")) params$nrun <- nrun_new 
if(exists("mnew")) params$m <- mnew
formulas <- readRDS("form_lists.rds")
metLists <- readRDS("met_lists.rds")
names(metLists)[3]

cat("\n\n>> number of imputations:", params$m, class(params$m))
cat("\t>> & methods to test:", met_list)
cat("\n\n>> bias to be calculated:", bias_names, "\n")
cat("\n\n>>>> date & time:", format(Sys.time(), "%d-%b %H:%M\n"))

#########################################################################################
#########################################################################################

#### moved data creation inside the loop so each run has new data
if(FALSE){
  z <- 1
  r <- "is_u"
  y <- 3
  
  r <- "HF_mis"
  params$deb <- TRUE
  met_list <- c("default", "rf","cc") # can use fewer methods to make it faster
  mods4sim
  for(m in seq_along(mnames)){
    # modnum = str_extract(mnames[m],"\\d+")
    mod    = mods4sim[m]
    mod <- mods4sim[3]
    fitReal <- glm(as.formula(paste0(r, mod)),
                   # data=ndGLM_scl_cc,
                   data=dat4sim,
                   family=binomial,
                   method=brglm2::brglm_fit)
    # ,
    coef(fitReal)
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
  metLists
  }
}

for(r in resp_list){
  # col_sel <- c(prVars,r) # columns to select, as strings
  col_list<- c(prVars,r )# columns to select, as strings
  form_list <- formulas[[r]]
  # metLists
  cat("\n\n********************************************************************************************")
  cat("\n>> response:", r,"\n\t& columns for imputation:", col_list)
  cat("\n********************************************************************************************\n\n")
  # }
  # imp_sim <- runSim(fullDat=dat4sim,col_sel = col_list,mets = met_list,forms=form_list, resp = r, vars = var_list, mods = mods4sim,par=params) # don't want to set seed
  imp_sim <- runSim(fullDat=dat4sim,col_sel = col_list,mLists = metLists,forms=form_list, resp = r, vars = var_list, mods = mods4sim,par=params) # don't want to set seed
  
  if(params$deb){
    cat("\n\n********************************************************************************************\n")
    cat(">>>>> BIAS VALUES: \n")
    cat("********************************************************************************************\n")
  }
  # bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,resp = r, vars = var_list, mod = mods4sim[z], mets = met_list, biasVals = bias_names, debug = params$deb)
  # bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,resp = r, vars = var_list, modnum = z, mets = met_list, biasVals = bias_names, debug = params$deb)
  bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,hdir = params$hdir,resp = r, vars = var_list, mods=mods4sim, mets = met_list, biasVals = bias_names, debug = params$deb, xdebug=params$xdeb)
}
# }
