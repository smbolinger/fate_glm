
  # col_sel <- c(prVars,r) # columns to select, as strings
source("fateGLM_impsim.R")
resp <- "HF_mis"
col_list<- c(prVars,resp )# columns to select, as strings
form_list <- formulas[[resp]]
# metLists
cat("\n\n********************************************************************************************")
cat("\n>> response:", r,"\n\t& columns for imputation:", col_list)
cat("\n********************************************************************************************\n\n")

res <- array(NA, dim = c(length(vars), length(mets), params$nrun, 3, length(mods4sim)))
# dimnames(res) <- list(c("pmm", "rf"),
dimnames(res) <- list(sort(as.character(vars)),
                      # c("pmm", "rf", "cart"),
                      as.character(mets),
                      as.character(1:params$nrun),
                      # c("estimate", "2.5 %","97.5 %","fmi"),
                      c("estimate", "2.5 %","97.5 %"),
                      names(mods4sim)
                      )

cat(sprintf(">>> running simulation %s times\n", params$nrun))
for(run in 1:params$nrun){
  cat(run)
  datNA <- mkSimDat(nd = dat4sim, seeed = run, vars=vars, method = "amp", wt = TRUE, xdebug=xdebug, debug = debug, convFact = TRUE)
  datNA <- datNA$amp
  
  for(x in seq_along(mets)){ # does matching by index help the trycatch statement?
    ## *~*~*~*~*
    # if (xdebug) cat("\n\n>>>> method:", x)
    skiptoNext <- FALSE
    
    tryCatch(
      expr = {
        vals <- mkImpSim(ampDat=datNA,
                         cols=col_list,
                         resp=resp, 
                         form_list =form_list,
                         vars=vars,
                         mods=mods4sim,
                         met=mets[x],
                         debug = params$debug,
                         m=params$m, 
                         xdebug=params$xdebug,
                         impplot=params$ipl
                         )
        # imp <- eval(parse(text=impCall))
        # skiptoNext <- FALSE
        # if(length(imp$loggedEvents > 0)) print(imp$loggedEvents)
      },
      error = function(e){
        cat("\nERROR:", conditionMessage(e), "\n")
        next
        # return(NULL)
        # skiptoNext <- TRUE
        skiptoNext <<- TRUE  # superassignment operator- not sure if necessary
        # imp <- list(imp=NA)
        # ret[,,y]
        # continue()
      }
    )
    if(skiptoNext) next
    
    vmatch <- match(rownames(vals), rownames(res)) # col 1 of vals is the row names
    res[vmatch, mets[x], run,,]  <- vals
  }
  if(run %% 100 == 0){
  # if(run %% 4 == 0){
    begn <- run-100
    endd <- run-0
    nowtime <- format(Sys.time(), "%d%b%H%M")
    fname <- paste(sprintf("out/runs%sto%s_%s.rds", begn, endd, nowtime))
    saveRDS(res[,,begn:endd,,], fname)
  }
  return(res)
}

# }
# imp_sim <- runSim(fullDat=dat4sim,col_sel = col_list,mets = met_list,forms=form_list, resp = r, vars = var_list, mods = mods4sim,par=params) # don't want to set seed
# if(debug) Rprof()
# imp_sim_m <- runSim(fullDat=dat4sim,col_sel = col_list,mLists = metLists,forms=form_list, resp = r, vars = var_list, mods = mods4sim,par=params) # don't want to set seed
# if(debug) Rprof(NULL)
# if(params$deb){
#   cat("\n\n********************************************************************************************\n")
#   cat(">>>>> BIAS VALUES: \n")
#   cat("********************************************************************************************\n")
# }
# # bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,resp = r, vars = var_list, mod = mods4sim[z], mets = met_list, biasVals = bias_names, debug = params$deb)
# # bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,resp = r, vars = var_list, modnum = z, mets = met_list, biasVals = bias_names, debug = params$deb)
# bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,hdir = params$hdir,resp = r, vars = var_list, mods=mods4sim, mets = met_list, biasVals = bias_names, debug = params$deb, xdebug=params$xdeb)
# }
# }
