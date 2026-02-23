
if(FALSE){
  datName <- "ndGLM_scl"
}

homeDir <- "C://Users/sarah/Dropbox/Models/fate_glm/"

missing_tab <- function(datName, prVars, dep = c("HF_mis", "is_u") ){
  ndDiag <- get(datName)
  expl = prVars
  
  # creeate an index of variables with at least one NA:
  exp_index <- lapply(expl, function(x) sum(is.na(ndDiag[x]))) > 0
  exp_miss <- expl[exp_index] 
  exp_miss
  
  tabNames <- list()
  # for(v in seq_along(exp_miss)){
  for(v in which(exp_index)){ # use the index for the vars withint expl
    # v=1
    # expl[v]
    # expl[-v]
    missTab <- ndDiag %>%
      dplyr::mutate(across(c(species, cam_fate), as.factor)) 
      
    # for this table, "dependent" is the independent var that is being compared
    missTab <- missTab %>%
      # finalfit::missing_compare(dependent = exp_miss[v], # this should be the table
      finalfit::missing_compare(dependent = expl[v], # this should be the table
      # finalfit::missing_pairs(dependent = expl[v],  # this is the plot
                                # explanatory = expl[expl!=v])
                                explanatory = expl[-v])
    # print(missTab)
    titl <- names(missTab)[1]
    names(missTab) <- c("var", "val", "not_missing", "missing", "p")
    
    missTabFancy <- missTab %>%
      gt::gt(rowname_col = "var") %>%
      gt::tab_header(title=titl) %>%
      gt::cols_label(val ~ "",
                 not_missing ~ "Not missing (percent)",
                 missing ~ "Missing (percent)",
                 p ~ "p-value") %>%
      # nestGLM::save_tab(suffix=paste(dName,dep,expl[v],uVar,sep="_"), rtf=TRUE)
      nestGLM::save_tab(suffix=paste(datName,expl[v],uVar,sep="_"), dir = homeDir,rtf=TRUE)
    
    tabNames[v] <- missTab
    # missPP <- list()
    for(i in seq_along(dep)){
      # missPP[i] <- ndDiag %>%
      missPP <- ndDiag %>%
        finalfit::missing_pairs(dependent = dep[i],
                                explanatory = expl,
                                position = "fill")
      now = format(Sys.time(), "_%m%d_%H%M_")
      # fname <- paste0(homeDir,"glm_script/figures/pp_",datName,dep[i],expl[v],uVar,now,".svg")
      fname <- paste0(homeDir,"figures/pp_",datName,dep[i],expl[v],uVar,now,".svg")
      ggsave(plot=missPP, filename = fname, device = "svg")
    }
    
    return(missTabFancy) # this returns the filename?
    # now = format(Sys.time(), "_%m%d_%H%M_")
    # fname <- paste0(homeDir,"glm_script/figures/pp_",datName,dep[i],expl[v],uVar,now,".svg")
    # ggsave(plot=missPP, filename = fname)
  }
}
