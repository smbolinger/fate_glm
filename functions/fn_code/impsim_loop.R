
for(z in seq_along(mods4sim)){
  # , cat("\n>>model:", names(model), model)
  cat("\n********************************************************************************************\n")
  cat("\n********************************************************************************************\n")
  cat("\n/////////////////////////////////////////////////////////////////////////////////////////////\n")
  cat("\n********************************************************************************************\n")
  cat("\n>>model:", names(mods4sim)[z], " | ", mods4sim[z])
  cat("\n********************************************************************************************\n")
  cat("\n/////////////////////////////////////////////////////////////////////////////////////////////\n")
  cat("\n********************************************************************************************\n")
  cat("\n********************************************************************************************\n")
  for(r in resp_list){
    col_sel <- c(prVars,r) # columns to select, as strings
    cat("\n\n********************************************************************************************\n")
    cat("\n********************************************************************************************\n")
    cat("\n>> response:", r,"\n\t& columns for imputation:", col_sel)
    cat("\n********************************************************************************************\n")
    cat("\n********************************************************************************************\n")
    # imp_sim <- runSim(datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = resp, vars = var_list, mod = mod4sim, nruns=nrun, debug = FALSE) # don't want to set seed
    # imp_sim <- runSim(dat=ndGLM_scl_cc, datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = r, vars = var_list, mod = mod4sim, nruns=nrun, debug = debug) # don't want to set seed
    # imp_sim <- runSim(datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = r, vars = var_list, mod = mod4sim, nruns=nrun, debug = debug) # don't want to set seed
    imp_sim <- runSim(datNA = sim_dat$amp,col_sel = col_sel,mets = met_list, resp = r, vars = var_list, mod = mods4sim[z], nruns=nrun, debug = debug) # don't want to set seed
    # bias_out <- parAvg(fullDat = ndGLM_scl_cc, impDat = imp_sim,resp = r, vars = var_list, mod = mod4sim,mets = met_list, biasVals = bias_names, debug = debug)
    # bias_out <- parAvg(fullDat = ndGLM_scl_cc, impDat = imp_sim,resp = r, vars = var_list, mod = mods4sim[z], mets = met_list, biasVals = bias_names, debug = debug)
    bias_out <- parAvg(fullDat = dat4sim, impDat = imp_sim,resp = r, vars = var_list, mod = mods4sim[z], mets = met_list, biasVals = bias_names, debug = debug)
    # biasfile <- paste0(params$home_dir, sprintf("out/bias_vals_%s_%s.rds", r, names(mods4sim)[z]))
    biasfile <- paste0(hdir, sprintf("out/bias_vals_%s_%s_%s.rds", r, names(mods4sim)[z], params$suffix))
    # saveRDS(bias_out, sprintf("out/bias_vals_%s_%s.rds",r, names(mods4sim)[z]))
    saveRDS(bias_out, biasfile)
    biasfile1 <- paste0(hdir, sprintf("out/bias_vals_%s_%s_%s.csv", r, names(mods4sim)[z], params$suffix))
    # biasfile1
    # write.csv(bias_out, file = sprintf("out/bias_vals_%s_%s.csv", r, names(mods4sim)[z]))# write to csv in case script aborts 
    write.csv(bias_out, file = biasfile1, row.names = FALSE)# write to csv in case script aborts 
    cat("\n\n********************************************************************************************\n")
    cat(">>>>> BIAS VALUES: \n")
    cat("********************************************************************************************\n")
    print(bias_out) # print the output to console
    # cat("\n******************************************************************************************\n")
  }
}