
# top_num <- aicTabImp$num[1]
# regtab <- function(aicTabImp, modList, howManyMod){
regtab <- function(impDat, aicTabImp, modList, debug=FALSE){
  # browser()
  
    modnum  <- as.numeric(rownames(aicTabImp)[m])
    # NOTE - will change based on whether there is a space in the modname
    intVar1 <- str_extract(string = modList[modnum], pattern = "\\w+(?=\\s\\*)")
    intVar2 <- str_extract(string = modList[modnum], pattern = "(?<=\\*\\s)\\w+")
    cat("\nmodel number", modnum,"=", modList[modnum], ", with these interaction vars:", intVar1, intVar2, ", and delta AIC:", aicTabImp$deltaAIC[m], "\n")
    fit <- with(impDat, glm(as.formula(paste(resp, modList[[modnum]])), family=binomial, method=brglm2::brglmFit)) 
    # pass results to tbl_regression before pooling, and it will handle pooling + tidying:
    vars=c('species', 'nest_age', 'obs_int', 'cam_fate', 'fdate')
    varNames=c('SPECIES', 'NEST_AGE', 'FINAL_INTERVAL', 'TRUE_FATE', 'END_DATE')
    vnames = as.list(varNames)    
    names(vnames) <- vars
    # tabOR <- gtsummary::tbl_regression(pool.fit, label=vnames, exponentiate=TRUE)
    summ.fit <- gtsummary::tbl_regression(fit, label=vnames, exponentiate=TRUE)
    # summ.fit
    # fn <- sprintf("_q1_reg_imp_%s_%s.rtf", datNameImp, now)
    headr <- paste("Pooled MI Regression Summary for Model:",
                   modnames(list(x = mods[[modnum]]), null=FALSE),
                   sep="\n")
    fn <- sprintf("%s_regtab_imp_%s-%s_%s.rtf", quest, datNameImp,modnum, now)
    summ.fit %>%
      gtsummary::as_gt() %>%
      gt::tab_header(title=headr) %>%
      gt::gtsave( filename=fn, path="analysis/" )
    # return(summ.fit)
    if (debug) print(summ.fit)
    return(fit)
  }
  # impTab <- impModTab(impModPool = impRes,modList = modList)

# ```{r eval=FALSE, warning=FALSE, include=FALSE}
# this works:

# fit <- with(impDat, glm(as.formula(paste(resp1, modList[[5]])), family=binomial, method=brglm2::brglmFit)) 
# use the number of the top model to evaluate it:
# fit <- with(impDat, glm(as.formula(paste(resp, modList[[top_num]])), family=binomial, method=brglm2::brglmFit)) 
# # pass results to tbl_regression before pooling, and it will handle pooling + tidying:
# vars=c('species', 'nest_age', 'obs_int', 'cam_fate', 'fdate')
# varNames=c('SPECIES', 'NEST_AGE', 'FINAL_INTERVAL', 'TRUE_FATE', 'END_DATE')
# vnames = as.list(varNames)    
# names(vnames) <- vars
# # tabOR <- gtsummary::tbl_regression(pool.fit, label=vnames, exponentiate=TRUE)
# summ.fit <- gtsummary::tbl_regression(fit, label=vnames, exponentiate=TRUE)
# summ.fit
# # fn <- sprintf("_q1_reg_imp_%s_%s.rtf", datNameImp, now)
# fn <- sprintf("_%s_reg_imp_%s_%s.rtf", quest, datNameImp, now)
# summ.fit %>%
#   gtsummary::as_gt() %>%
#   gt::gtsave( filename=fn, path="analysis/" )


# ```
