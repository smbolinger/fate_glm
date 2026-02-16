

  # col_sel <- c(prVars,r) # columns to select, as strings
# source("fateGLM_impsim.R")

library(brglm2) # penalized logistic regression
library(CALIBERrfimpute) # could access using ::
library(gt)
library(gtsummary)
library(data.table)
suppressMessages(library(mice))
suppressMessages(library(tidyverse))
# look into tidytable for tidyverse syntax w/ data.table
options(width=99)
#if(params$deb|params$xdeb) func = "debug_imp_sim_func.R" else func = "imp_sim_functions.R"
#source(func)
#source("mice_functions.R")
#source("imp_sim_functions.R")
#source("missing_data.R")
#source("other_imp.R")
#source("gen_sim.R") ### new data simulation function

#########################################################################################

params <- list(nrun=100,
	       hdir="/home/wodehouse/Projects/fate_glm/",
	       #outdir = paste0(format(Sys.time(), "%d%b"),"/"),
               suffix = "",
	       outdir = format(Sys.time(), "%d%b"),
               ampWt=3,
               deb=FALSE,
               xdeb=FALSE, # for obsessively checking things - not useful for normal debugging & lots of text output
               ipl=FALSE,
               win=FALSE,
               test=FALSE,
               seeds = c(666),
               # resp = NULL,
               j = 50,
               m=20)

arg <- commandArgs(trailingOnly=TRUE)
# Rscript isim.R win test r3 s613 m5 j1
if(length(arg)==0){
    cat("\n\n/////////////////////////////////////////////////////////////////////////////////////////////\n")
    cat("** NOTE ** no arguments provided - using default of windows = FALSE\n")
    cat("/////////////////////////////////////////////////////////////////////////////////////////////\n\n")
}else if(length(arg) > 0){
    cat("\n\n/////////////////////")
    cat("  arg =  ", paste(unlist(arg), collapse=" ; "))
    cat("  ////////////////////////////////\n")
  for(a in arg){
	  if(grepl("r\\d+$", a)) params$nrun  <- as.numeric(str_extract(a, "\\d+"))
	  else if(a=="win") params$win        <- TRUE
	  else if(a=="deb") params$deb        <- TRUE
	  else if(a=="xdeb") params$xdeb        <- TRUE
	  else if(a=="ipl") params$ipl       <- TRUE
	  # else if(a=="is_u" | a == "HF_mis") params$resp       <- a
          else if(a=="test") params$test <- TRUE
	  else if(grepl("suff_\\w+", a)) params$suffix <- as.numeric(str_extract(a, "(?<=suff_)\\w+"))
	  else if(grepl("aw\\d", a)) params$ampWt <- as.numeric(str_extract(a, "\\d"))
	  else if(grepl("j\\d+", a)) params$j <- as.numeric(str_extract(a, "\\d+"))
	  else if(grepl("s\\d+", a)) params$seeds <- c(as.numeric(str_extract(a, "\\d+")))
	  else if(grepl("m\\d+", a)) params$m <- as.numeric(str_extract(a, "\\d+"))
  }
}

#########################################################################################

s_files <- c("mice_functions.R", "missing_data.R", "other_imp.R", "imp_sim_functions.R")
if(params$deb|params$xdeb) s_files[4] = "debug_imp_sim_func.R" 
cat("\n<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>\n")
#cat("\n###################################################################")
cat("\n>>> sourcing files:", paste(s_files,collapse=" ; "))
#lapply(s_files, function(x) invisible(source(x)))
invisible(lapply(s_files, function(x) source(x)))

#########################################################################################

bprint <- function(x) print(rbind(head(x, 3), tail(x,3)))
debugging <- FALSE # for uickly setting values when working in the file with the functions
if(params$win){
    cat("\n[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]\n")
    cat("\n>>>> date & time:", format(Sys.time(), "%d-%b %H:%M")) 
    cat("\n[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]\n") 
    params$hdir <- "C:/Users/sarah/Dropbox/Models/fate_glm/"
}
if(params$j > params$nrun) stop("j must be less than or equal to nrun!")
# if(is.null(params$resp)) stop("no response variable specified!")
if(params$test){
    #mets <- c("cc", "cart", "caliber", "passive")
    params$outdir <- paste("t",params$outdir,sep="-")
}

suffix <- paste(sprintf("%sruns", params$nrun), params$suffix, sep="_")
now_dir <- paste(params$hdir, params$outdir, sep="out/")
if(!dir.exists(now_dir)) dir.create(now_dir)

seed_out <- FALSE
if(is.null(params$seeds)){
    seed_out <- TRUE
    params$seeds <- c(71358, 102891, 82985, 61389, 11153)
    last_seed <- as.numeric(grep(readLines("seed.flag"), params$seeds))
    cat("last seed:", last_seed)
#new_order <- params$seeds + last_seed
    new_order <- seq(1,length(params$seeds)) + last_seed
    cat("new order:", new_order)
    new_order[new_order > 5] = new_order[new_order > 5] - 5
    cat("new order:", new_order)
    params$seeds <- params$seeds[new_order]
    cat("\n****************************************************************************")
    cat("\n>>> using default seed list, in new order:", params$seeds, "\t")
}


modList <- readLines("modList.txt")
mods4sim <- modList[c(1,8,16) ]
names(mods4sim) <- c("m1", "m8", "m16")
fits <- readRDS("fits.rds")
mnums <- rep(c("1", "8", "16"))
cat("model nums from file:", mnums)

fullDat <- read.csv("dat_complete.csv", stringsAsFactors = TRUE)
fullDat$cam_fate <- relevel(fullDat$cam_fate, ref="H") # make 'H' the reference category
fullDat$species <- relevel(fullDat$species, ref="LETE")
levels(fullDat$HF_mis) <- c(0,1)
levels(fullDat$is_u)   <- c(0,1)
cat("\n>>>> releveled categorical variables")


#### for the imputation: ####################################################################
formulas <- readRDS("form_lists.rds")
metLists <- readRDS("met_lists.rds")
#cat("\n>> methods for individual variables:\n")
#print(metLists)

#### for creating the simulated data: #######################################################
betas <- readRDS("betas.rds") # this is a list of vectors or something
mList <- readRDS('means.rds')
cMat  <- readRDS('cormat.rds')
fprob <- c( 'H'=0.53,'A'=0.1, 'D'=0.13,'F'=0.06, 'Hu'=0.13,'S'=0.06 )
sprob <- c('CONI'=0.32, 'LETE'=0.68)
nnest <- ifelse(params$test==T, 200, 200) # number of simulated nests
mpatt  <- readRDS("misPatt.rds")
awFile <- case_when(params$ampWt==1 ~ "ampWts.rds",
                    params$ampWt==2 ~ "ampWts2.rds",
                    params$ampWt==3 ~ "ampWts3.rds")
ampwt <- readRDS(awFile)
cat("\n\n<><><><><><><><><><> Missingness pattern & variable weights: <><><><><><><><><><><><><><><><><><><><>\n")
print(mpatt)
print(ampwt)

#########################################################################################

#cat("sort vars or not???")
vars <-  c("nest_age", "cam_fateD", "cam_fateA", "cam_fateF", "cam_fateHu", "cam_fateS", "speciesCONI", "speciesCONI:nest_age", "speciesCONI:obs_int", "obs_int", "fdate") # all vars
prVars <- c("species", "cam_fate", "obs_int", "nest_age", "fdate")
mets <- c("default", "cart", "caliber","passive", "stratify","cf_cc","cc")# don't need full here?
resp_list <- c("is_u", "HF_mis")

#form_list <- formulas[[params$resp]]
#########################################################################################

cat("\n<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>\n")
#cat("\n<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>\n")
cat("\n>> METHODS:", mets)
cat(sprintf("\t>> TOTAL REPS: %s x %s WITH %s NESTS EACH", length(params$seeds), params$nrun, nnest)) 
if(params$deb) cat("\t- debug ON")
if(params$xdeb) cat(" + EXTRA")
#cat("\t>> response:", params$resp,"\n\t& cols for imputation:", col_list)
#cat("\n\n****************************************************************************")
cat("\n>> output will be saved every", params$j, "runs to dir:", now_dir,"\n\n")

for (seed in params$seeds){
    outf <- paste0(now_dir,sprintf("/%s_loggedEvents.out",seed ))
    #if(params$test) outf <- paste0(now_dir,sprintf("/%s_loggedEvents-TEST.out",seed ))
    #res <- array(NA, dim = c(length(vars), params$nrun, 3, length(mods4sim), length(resp_list)))
    res <- array(NA, dim = c(length(vars), params$nrun, length(mods4sim), length(resp_list)))
    # dimnames(res) <- list(sort(as.character(vars)),
    dimnames(res) <- list(as.character(vars),# need to be in same order as vals
                          as.character(1:params$nrun),
                          #c("Estimate", "2.5 %","97.5 %"),
                          names(mods4sim),
                          resp_list
                          )
    camFateVars <- vars[grepl(pattern="cam_fate", x=vars, fixed=TRUE)]
    # keep a count of sample size for each category
    varInfo <- array(NA, dim=c(length(camFateVars), params$nrun, length(mods4sim)))
    dimnames(varInfo) <- list( camFateVars, seq(1,params$nrun), mods4sim)

#cat("\n<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>\n")
    cat(sprintf(">>>>>>>>>>>>>> running simulation %s times \t>>> seed = %s ", params$nrun, seed))
    cat("\t>> & no. imp.:", params$m, "\n\n")
    for(mod in seq_along(mods4sim)){
        # select the correct list of formulas and list of beta values!
        #cat("\n()()()()()() MODEL", mods4sim[mod], " ()()()()()()()()()()()()()()()() \n\n") ## *~*~*~*~*
        if (params$deb) cat("\n\n::::::::::::::::::: MODEL", mods4sim[mod], ":::::::::::::::::::::::::::::::\n\n") ## *~*~*~*~*
        beta_list <- betas[[mod]]
        form_list <- formulas[[names(mods4sim)[mod]]]
        modnum <- gsub("\\D+", "", names(mods4sim)[mod])
        if (params$deb) cat("\n<> <> model number:", modnum)
        #fit_real <- fits[mnums==modnum]
        fit_isU <-  glm(as.formula(paste("is_u", mods4sim[mod])), family=binomial, data=fullDat, method=brglm2::brglmFit)
        fit_mis <-  glm(as.formula(paste("HF_mis", mods4sim[mod])), family=binomial, data=fullDat, method=brglm2::brglmFit)
        if (params$deb) cat("\n>> formula list, beta list, model fits:") ## *~*~*~*~*
        if (params$deb) qvcalc::indentPrint(names(form_list))
        if (params$deb) qvcalc::indentPrint(str(beta_list))
        if (params$deb) qvcalc::indentPrint(summary(fit_isU))
        if (params$deb) qvcalc::indentPrint(summary(fit_mis))
        #print(sapply(fit_real, head))
        #print(class(fit_real))
        #print(length(fit_real))
        for(run in 1:params$nrun){
            cat(run)
            # repeat this until you get a dataset w/o missing levels??
            #dat4sim <- mkSim(resp_list, mods4sim[mod], nnest, cMat, mList, beta_list, fprob, sprob, prList, debug=params$deb)
            #dat4sim <- mkResp(resp_list, mods4sim[mod], nnest, cMat, mList, beta_list, fprob, sprob, prList, debug=params$deb)
            dat4sim <- mkResp(resp_list, mods4sim[mod], nnest, cMat, mList, beta_list, fprob, sprob, prList, debug=FALSE)
            if (params$xdeb) cat(mod)
            # if you do summary, you get the standard error and all that as well
            if (params$xdeb) cat("\n<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>\n")
            if (params$xdeb) cat("\n> fitting simulated data:")
            #fitSim1 <- summary(glm(as.formula(paste0("is_u",mods4sim[mod])),
            fitSim1 <- glm(as.formula(paste0("is_u",mods4sim[mod])),
                           family=binomial,
                           data=dat4sim,
                           method=brglm2::brglmFit)
            #fitSim2 <- summary(glm(as.formula(paste0("HF_mis",mods4sim[mod])),
            fitSim2 <- glm(as.formula(paste0("HF_mis",mods4sim[mod])),
                           family=binomial,
                           data=dat4sim,
                           method=brglm2::brglmFit)


            #cat("\n > > classes: ", class(fit_real[[1]]), class(fitSim1))
            if (params$xdeb) cat("\n+ + + coefs from real data vs from sim data:\n")
            #qvcalc::indentPrint(sapply(fit_real, function(x) coef(x)))
            #qvcalc::indentPrint(data.frame("real"=coef(fit_isU), "sim"=coef(fitSim1)[,"Estimate"]))
            #qvcalc::indentPrint(data.frame("real"=coef(fit_mis), "sim"=coef(fitSim2)[,"Estimate"]))
            if (params$xdeb) qvcalc::indentPrint(data.frame("real"=coef(fit_isU), "sim"=coef(fitSim1), "difference"=coef(fit_isU)-coef(fitSim1)))
            if (params$xdeb) qvcalc::indentPrint(data.frame("real"=coef(fit_mis), "sim"=coef(fitSim2), "difference"=coef(fit_mis)-coef(fitSim2)))
            #print(coef(fit_mis)[-1])
            
            #print(coef(fitSim2)[-1])


            if (params$xdeb) cat("\n + + + calculate & fill in the bias:")
            #print(coef(fit_isU)[-1] - coef(fitSim1)[-1])
            vlist1 <- colnames(model.matrix(as.formula(paste0("is_u", mods4sim[mod])),data = fullDat))[-1]
            vlist2 <- colnames(model.matrix(as.formula(paste0("HF_mis", mods4sim[mod])),data = fullDat))[-1]
            if (params$xdeb) cat("\n>>>> var lists:", vlist1, vlist2)
            res[vlist1,run,mod,"is_u"] <- coef(fit_isU)[-1] - coef(fitSim1)[-1]
            res[vlist2,run,mod,"HF_mis"] <- coef(fit_mis)[-1] - coef(fitSim2)[-1]
            if (params$xdeb) qvcalc::indentPrint(ret[,run,mod,])
            #print(coef(fit_isU))
            #print(coef(fit_mis))
            #cat("\n+ + + coefs from sim data:")
            #print(coef(fitSim1))
            #print(coef(fitSim2))
            #qvcalc::indentPrint(sapply(c(fitSim1, fitSim2), function(x) coef(x)))
        }
    }
    if (params$xdeb) cat("\n>> res, all filled in:")
    if (params$xdeb) qvcalc::indentPrint(res, indent=8)

}

#saveRDS(res, "sim_bias_.rds")
fnamee <- paste0(now_dir, sprintf("/sim_bias_%s.rds", seed))
saveRDS(res, fnamee)

            
