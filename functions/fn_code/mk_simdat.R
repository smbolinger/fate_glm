
mkSimDat2 <- function(seeed, nd, vars, facToNum=FALSE, method="amp", wt=TRUE, debug=FALSE, xdebug=FALSE, convFact=FALSE){
    if(method=="amp"){
        cat("\n>>> making amputed data\n") # ~*~*~*
        #nd <- as.matrix(nd)
        # dat4amp <- add_dummy(as.matrix(nd), debug=debug)
        dat4amp <- add_dummy(nd, debug=debug)
        set.seed(seed=seeed)
        bprint(dat4amp)# ~*~*~*
        #suppressWarnings(amp_out1 <- mice::ampute(dat4amp))
        suppressMessages(suppressWarnings(amp_out1 <- mice::ampute(dat4amp)))
        bprint(amp_out1$amp)# ~*~*~*

        if(FALSE){
            out <- add_fact(amp_out1$amp,facToNum = T, debug=T)
            # levels(out$cam_fate)
            table(out$cam_fate)
            xtabs(formula = ~ cam_fate + species, data = out)
            xtabs(formula = ~ cam_fate + species, data = nd)
        }
        new_patt <- amp_out1$patterns
        # no_miss <- c("obs_int", "fdate", "is_u", "speciesLETE", "speciesCONI")
        no_miss <- c("obs_int", "fdate", "is_u", "speciesCONI")
        # The problem with doing this with indices is that (as just happened), I forget how
        # I did things and then add new variables w/o accounting for them     # Easier to reference names?
        is_miss <- names(new_patt)[!names(new_patt) %in% no_miss]
        is_miss

        # the order: missing age only; missing fate/HF_mis only; missing both
        miss_pat <- list(c(0, rep(1,7)), c(1,rep(0,7)), c(rep(0,8)) )
        miss_pat

        m <- do.call(rbind,miss_pat) # create the matrix of the missingness patterns 
        p <- do.call(rbind, rep(list(c(rep(1,4))), 3)) # create additional columns for the vars not missing values

        new_prop <- 0.16
        miss_patt_mat <- cbind(m,p)
        colnames(miss_patt_mat) <- c(is_miss, no_miss)
        saveRDS(miss_patt_mat, "misPatt.rds")
        patt_freq <- c(0.45,0.45,0.1) # missing: age only, fate only, both
        ### *~*~*~*~* #######
        #if(debug & seeed==1){
        # rr=0
        # if(debug & rr<1){
        #     cat("\n\n>> missingness matrix:\n")
        #     print(miss_patt_mat)
        #     cat("\n>> proportion missing:", new_prop, "\n\n>> frequency of each pattern:", patt_freq)
        #     rr = rr +1
        # }
        new_order <- c(is_miss, no_miss)
        ### *~*~*~*~* #######
        if(debug) cat("\n\n>> reorder columns:", new_order) # ~*~*~*
        dat4amp <- dat4amp %>% select(all_of(new_order)) # reorder the columns to match the matrix

        suppressWarnings(amp_out <- mice::ampute(dat4amp, prop = new_prop, patterns = miss_patt_mat, freq = patt_freq))
        if(FALSE){
            out <- add_fact(amp_out$amp,facToNum = T, debug=T)
            # levels(out$cam_fate)
            table(out$cam_fate)
            xtabs(formula = ~ cam_fate + species, data = out)
        }
        ### *~*~*~*~* #######
        # if(xdebug) cat("\n\nCreate new missing values:\n") # ~*~*~*
        # if(xdebug) print(mice::md.pattern(amp_out$amp, rotate.names = TRUE)) # ~*~*~*

        # 1 = complete; 0 = has missings
        wts <- amp_out$weights 
        wts
        wts[1,] <- c(0, 0, 0, 0.8, 0.2, rep(0, 7)) # missing age only - what vars contribute
        # saveRDS(wts, "ampWts.rds")
        # wts[3,] <- c(0, 0, 0, 0.8, 0.2, rep(0, 8))
        ### *~*~*~*~* #######
         if(debug){ # ~*~*~*
           cat("\n>> new weights:\n")
           print(wts)
           cat("\n>> to go with the patterns:\n")
           print(miss_patt_mat)
         }
        # what exactly are the weights doing?
        suppressWarnings(amp_out_wt <- mice::ampute(dat4amp, prop = new_prop, patterns = miss_patt_mat, freq = patt_freq,weights = wts))

        if(FALSE){
            out <- add_fact(amp_out_wt$amp,facToNum = T, debug=T)
            # levels(out$cam_fate)
            table(out$cam_fate)
            table(out$HF_mis)
            table(out$is_u)
            xtabs(formula = ~ cam_fate + species, data = out)
        }
        ### *~*~*~*~* #######
        # if(debug) cat("\n\nCreate more new missing values, with weighted probabilities:\n") # ~*~*~*
        # if(debug)  print(mice::md.pattern(amp_out_wt$amp, rotate.names = TRUE)) # ~*~*~*
        # missing_tab("amp_out_wt", prVars)

        # datList <- ifelse(wt, amp_out_wt, amp_out) # ifelse is not what I need here
        if(wt) {datList <- amp_out_wt} else {datList <- amp_out}
        ### *~*~*~*~* #######
        if(debug) print(names(datList)) # ~*~*~*
        if(debug) bprint(amp_out_wt)
        #if(debug) print(str(datList$amp)) # ~*~*~*
        # if (debug) print(class(datList$amp)) # ~*~*~*
        # cat("\n WHAT IF WE LEAVE THEM AS DUMMIES?\n")
        # if (convFact) datList$amp <- add_fact(dat = datList$amp, facToNum=facToNum, debug=debug) # could probably reference the global debug instead...
        # datList$amp <- datList$amp %>%
        # levels(datList$amp$cam_fate)
        # str(datList)
        # datList
        # return(as.matrix(dat))
        return(datList)

    } else {

    ageNA <- sample(x=nd$nest, size = 10, replace = FALSE)
    nd$nest_age[!is.na(match(nd$nest,ageNA))] = NA
    fateNA <- sample(x=nd$nest, size = 12, replace = FALSE)
    nd$cam_fate[!is.na(match(nd$nest,fateNA))] = NA

    # ### *~*~*~*~* #######
    # if (debug){ # ~*~*~*
    #   cat("new number missing nest_age:", sum(is.na(nd$nest_age)),
    #       "new percent missing nest_age:", sum(is.na(nd$nest_age)) / nrow(nd),
    #       "original percent missing:", sum(is.na(ndGLM_scl_all$nest_age)) / nrow(ndGLM_scl_all) )
    #   cat("\nnew number missing cam_fate:", sum(is.na(nd$cam_fate)),
    #       "new percent missing cam_fate:", sum(is.na(nd$cam_fate)) / nrow(nd),
    #       "original percent missing:", sum(is.na(ndGLM_scl_all$cam_fate)) / nrow(ndGLM_scl_all) )
    #   
    # }
    return(nd)
  }
}
