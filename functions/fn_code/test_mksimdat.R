
########################################################################################
##### MAKE SIMULATED DATA ###############################################################
########################################################################################

## *NOTE* IF YOU CHANGE THINGS HERE, CHANGE THEM IN imp_sim_functions.R AS WELL!!*

source("imp_sim_functions.R")

if (FALSE){
    # nd <- ndGLM_scl_cc
    nd <- dat4sim
    wt <- TRUE
    convFact <- TRUE
    debug <- params$deb
    debug <- TRUE
    xdebug <- TRUE
    # facToNum <- TRUE
    facToNum <-FALSE 
    # vars <- var_list
    seeed <- 614
    wts <- ampwt
    new_prop=0.16
    patt_freq=c(0.45,0.45,0.1)
    
}

########################################################################################
##### IMPUTE/FIT/POOL SIM DATA ##########################################################
########################################################################################

if(debugging){
    # dat=mkSimDat(ndGLM_scl_cc, convFact = TRUE)$amp
    # ampDat=mkSimDat(ndGLM_scl_cc, convFact = TRUE)$amp
    s = 614
    s=618
    ampDat = mkSimDat(seeed=s, nd=dat4sim, vars=var_list, convFact=TRUE)$amp
    xtabs(~ cam_fate + species, data=ampDat)
    # ampDat <- datList$amp
    # ampDat <- datNA
    # aDat = sim_dat$amp
    # aDat = sim_dat$amp[1:20,]
    resp="is_u"
    # resp = "HF_mis"
    # resp="isu"
    # resp = "HFmis"
    mod=modList[1]
    mods = mods4sim
    m=20
    met="pmm"
    met="rf"
    met="default"
    # met="cc"
    # met = "caliber"
    met="passive"
    met = "stratify"
    met = "cf_cc"
    mets <- met_list
    # seed=61389

    fam=binomial
    regMet="brglm_fit"
    iter=500
    # cols <- col_list
    debug = TRUE
    xdebug=TRUE
    y=1
    # why only these vars?
    # vars= c("nest_age", "cam_fateD", "cam_fateA", "cam_fateF", "cam_fateHu", "cam_fateS", "speciesLETE", "speciesLETE:nest_age")
    modd <- mods4sim[mod]
    # vars <- var_list
    dat4sim <- mkSim(resp_list, mods4sim[mod], nnest, cMat, mList, beta_list, fprob, sprob, prList, debug=params$deb)
    convFact <- TRUE
    datNA <- mkSimDat( seeed = run+seed, nd = dat4sim, mpatt=mpatt, wts=ampwt, xdebug=params$xdeb, debug = params$deb, convFact=convFact)
    ampDat <- datNA$amp
    impplot = TRUE
    fcToNum <- TRUE
    r <- 1
    m <- 5
    # pr_list <- prVars
    vlist <- colnames(ampDat)
    met_list <- metLists[,,,mod]
    form_list <- formulas[[names(mods4sim)[mod]]]
    # mod = modList[1]
    # ampDat <- dat
    # ampDat <- datList$amp
}

