
# create the interaction plots
grpColor <- c(LETE="#AA4499", CONI="#44AA99")
intPlot <- function(dat, 
                    modnum, 
                    plot_type = "pred", 
                    vars=c("species", "nest_age"),
                    y_lab = "Predicted probability of\n misclassification",
                    x_lab = "Nest age - centered (days)",
                    # grid=intGrid1 # probably shouldn't have the same name as a function?
                    # grid1=intGrid1 # probably shouldn't have the same name as a function?
                    grid1
                    ){
  
    model   <- with(dat,
                    glm(as.formula(paste0(resp, modList[[modnum]])),
                    family=binomial,
                    method=regMet
                    ))
    
    #  from the documentation: 
    #  Warning: Slopes and elasticities can only be calculated for continuous numeric variables. The slopes() functions will automatically revert to comparisons() for binary or categorical variables.
    #  
    #  Why are all the predictions only a few values?
    # intPred <- marginaleffects::predictions(model,
    #                                         # variables=c("species", "nest_age"),
    #                                         variables=c("species"), # try just including X
    #                                         newdata=grid1
    #                                         # so was this always working with these 2 variables?
    #                                         # I guess it was ignoring it when it was "nest_age"
    #                                         # except when the grid also varied nest_age, 
    #                                         # which is why the species*nest_age interaction plot
    #                                         # wasn't working
    #                                         # but anyway, I don't think I need the variables arg?
    #                                         # nope, still not working for the species*nest_age interaction
    #                                         # and the CI are much narrower now for the ones that work
    #                                         # could be because I changed the interaction order?
    #                                         # why does nest_age*species work, but not the reverse?
    #                                         # and why is the other one obs_int*species, not the reverse?
    #                                         # changing the order without putting the variables
    #                                         # argument back in didn't help anything...
    #                                         # variables=vars
    #                                         )
    
    # this one gives a list as output:
    # plDat   <- ifelse(plot_type=="slope",as.data.frame(intSlope),as.data.frame(intPred))
    # plDat <- intPred
    # if(plot_type=="slope") plDat <- intSlope
    # the confidence intervals in this plot don't seem wide enough...
    # plDat <- case_when(plot_type == "pred" ~ intPred,
    #                    plot_type == "slope" ~ intSlope)
    # ggplot(intPred,
    mm <- sym(vars[1])
    xx <- sym(vars[2])
    # colores <- c("")
    # g <- ggplot(plDat,
    
    # I didn't change anything about the ggplot function call
    # or really anything about this function definition, except for making it so that
    # the variables used for predictions aren't always species & nest age 
     if(plot_type=="slope"){
      
      intSlope <- marginaleffects::slopes(model,
                                          variables=c("species"),
                                          # variables=vars,
                        # variables=c("species", "obs_int"),
                        newdata=grid1)
      pl <- ggplot(intSlope,
             aes(x=!!xx, y=estimate, ymin=conf.low, ymax=conf.high,color=!!mm)) +
        labs(x=x_lab, y=y_lab)+
        scale_color_manual(values=grpColor,
                           labels=c("Common\nNighthawk", "Least Tern"),
                           name="Species") +
        geom_ribbon(alpha=0.2) +
        geom_line() +
        theme_classic() +
        theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20), # can't do margins for x and y axis titles at once
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),
        axis.title.y = element_text(margin=margin(r=5)),
        axis.title.x = element_text(margin=margin(t=5))) 
        
    } else {  # ggplot(plDat,
      intPred <- marginaleffects::predictions(model,
                                              # variables=c("species", "nest_age"),
                                              variables=c("species"), # try just including X
                                              newdata=grid1
      )  
      pl <- ggplot(intPred,
             # aes(x=obs_int, y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
             # aes(x=as.numeric(obs_int), y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
             aes(x=!!xx, y=estimate, color=!!mm)) +
        # geom_point() +
        labs(x=x_lab, y=y_lab)+
        scale_color_manual(values=grpColor,
                           labels=c("Common\nNighthawk", "Least Tern"),
                           name="Species") +
        geom_smooth(method=NULL) +
        theme_classic() +
        theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20), # can't do margins for x and y axis titles at once
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),
        axis.title.y = element_text(margin=margin(r=5)),
        axis.title.x = element_text(margin=margin(t=5))) 
        # geom_ribbon(alpha=0.2) +
        # geom_line()
      
    }
    
    return(pl)
}


# create_grid <- function(dat, var){
intPredict <- function(model, var){

  # nm <- paste("intGrid_", var)
  # grid
  # assign(x = nm, value=marginaleffects::datagrid(newdata=ndGLM_scl, grid_type="balanced"))
  # return(nm)
  # model   <- with(dat,
  #                 glm(as.formula(paste0(resp, modList[[modnum]])),
  #                 family=binomial,
  #                 method=regMet
  #                 ))
  # intPred <- 
  # grid1 <- marginaleffects::datagrid(newdata=ndGLM_scl, grid_type="balanced")
  # grid1 <- get(nm)
  intPred <- marginaleffects::predictions(model, newdata=grid1)
}

# if(grepl("*", modList[top_num], fixed=TRUE)){
# var <- "obs_int"
# # intGridNew1 <- marginaleffects::datagrid(newdata=ndGLM_scl,


if(grepl("*", modList[top_num], fixed=TRUE)){ # the defaults are for this model
  # change the grid used based on which variables are in the interaction:
  if(intVar2 == "obs_int"){
    # gr = intGridObs 
    grName = "intGridObs"
    lab = "Final interval - centered (days)"
  } else {
    # gr = intGridAge
    grName = "intGridAge"
    lab = "Nest age - centered (days)"
  }

  intPlot(impDat,                             # (interaction between species & nest_age)
          # modnum=5)
          modnum=top_num,
          vars = c(intVar1, intVar2), # in this case, intVar1 should always be species
          # grid1=gr,
          grid1=get(grName),
          x_lab=lab,
          y_lab=yl)
  # ,
          # y_lab="Predicted probability of\n unknown field fate")                   
}
# g <- get(grName)
# rm(g)

if(grepl("*", modList[sec_top], fixed=TRUE)){
  # if("obs_int" %in% c(intVar1.2, intVar2.2)){
  if(intVar2.2 == "obs_int"){
    # gr = intGridObs
    grName = "intGridObs"
    lab = "Final interval - centered (days)"
  } else {
    # gr = intGridAge
    grName = "intGridAge"
    lab = "Nest age - centered (days)"
  }
  
  intPlot(impDat,
            # modnum=4, 
            modnum=sec_top, 
            # grid1=gr, 
            grid1=get(grName), 
            # vars=c("species", "obs_int"), 
            vars=c(intVar1.2, intVar2.2), 
            x_lab=lab,
          y_lab = yl)
}



# ```{r eval=FALSE, include=FALSE}
# # modList[[5]]
# # mod5 <- paste0("HF_mis ", modList[[5]])
# # mod5
# impDatsmall2 <- imputeDat(imppDat, m=10, return="complete")
# modFit <- lapply( impDatsmall2, function (x) {
#   glm(HF_mis ~ nest_age * species + fdate + obs_int + cam_fate, 
#       data = x, 
#       family = binomial,
#       method = brglmFit)}
# )
# 
# pred <- lapply(modFit, function(x){
#   # Error: This command will generate many estimates. Set `options(marginaleffects_safe=FALSE)` to circumvent this guardrail.
#   # marginaleffects::predictions(x, newdata=fullGridAge, hypothesis=~pairwise)
#   marginaleffects::predictions(x, newdata=fullGridAge)
# })
# # pred$term <- pred$hypothesis
# # the pool function needs a column named "term", but which column? the example is confusing
# predPool <- mice::pool(pred)
# ```
