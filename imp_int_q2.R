
# for question 2
# try not using the function at all

# grid1 <- intGridAge
modnum <- 4
# dat <- impDat
dat <- impInt
vars <- c("species", "nest_age")
y_lab="Predicted Probability of \nMisclassification"
x_lab="Nest Age in Days (centered)"
mm <- sym(vars[1])
xx <- sym(vars[2])
grpColor <- c( CONI="#44AA99",LETE="#AA4499")

model   <- with(dat, glm(as.formula(paste0(resp, modList[[modnum]])),
                         family=binomial,
                         method=regMet
                         )
                )

grid1 <- fullGridAge
# this function is supposed to take imputations as input, but I don't think it calculates the
# SE correctly. try creating a list of model fits for each of m datasets, and then pooling

# this was just working and now it throws a missing std.error column error!
# but only for model 4. weird. nevere mind. just had misplaced parentheses
intPred <- marginaleffects::predictions(model,
                                        # variables=c("species", "nest_age"),
                                        # variables=c("species"), # try just including X
                                        # type="response", # need this to use glm.fit method?
                                        newdata=grid1
)
# Error: This function does not support multiple imputation. Call `predictions()` or `avg_predictions()` instead. These functions return easy to plot data frames.
# intPredPlot <- marginaleffects::plot_predictions(model,
#                                                  newdata=grid1)

pl <- ggplot(intPred,
       # aes(x=obs_int, y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
       # aes(x=as.numeric(obs_int), y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
       aes(x=nest_age, y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
       # aes(x=!!xx, y=estimate, color=!!mm)) +
       # aes(x=nest_age, y=estimate, color=species)) +
  # geom_point() +
  labs(x=x_lab, y=y_lab)+
  coord_cartesian(ylim=c(0,1))+ 
  scale_color_manual(values=grpColor,
                     labels=c("Common\nNighthawk", "Least Tern"),
                     name="Species") +
  # geom_smooth(method=NULL) +
  geom_line(linewidth=1) +
  geom_ribbon(alpha=0.2)+
  theme_classic() +
  theme(axis.text = element_text(size=18),
  axis.title = element_text(size=20), # can't do margins for x and y axis titles at once
  legend.text=element_text(size=16),
  legend.title=element_text(size=18),
  axis.title.y = element_text(margin=margin(r=5)),
  axis.title.x = element_text(margin=margin(t=5))) 
# pl1 <- intPlot(impDat,                             # (interaction between species & nest_age)
# # intPlot(impDat,                             # (interaction between species & nest_age)
#           # modnum=5)
#           modnum=5,
#           # plot_type="slope",
#           vars = c("species", "nest_age"), # in this case, intVar1 should always be species
#           # grid1=gr,
#           grid1=intGridAge,
#           x_lab="Nest age - centered (days)",
#           y_lab="Predicted probability of \nmisclassification")
pl

# I got this warning before, but I forget why...
# Warning :[38;5;111mFailed to fit group 1.[39m Caused by error in `smooth.construct.cr.smooth.spec()`: [33m![39m x has insufficient unique values to support 10 knots: reduce k.
# Warning :[38;5;111mFailed to fit group 2.[39m Caused by error in `smooth.construct.cr.smooth.spec()`: [33m![39m x has insufficient unique values to support 10 knots: reduce k.
# and maybe this one too:
# Warning :[38;5;111mNo shared levels found between `names(values)` of the manual scale and the data's [32mcolour[38;5;111m values.[39m

# for question 2
# pl2 <- intPlot(impDat,                             # (interaction between species & nest_age)
#           # modnum=5)
#           modnum=4,
#           vars = c("species", "obs_int"), # in this case, intVar1 should always be species
#           # grid1=gr,
#           grid1=intGridObs,
#           x_lab="Final interval - centered (days)",
#           y_lab="Predicted probability of \nmisclassification")
# pl2
modnum <-10 
dat <- impInt
vars <- c("species", "obs_int")
y_lab="Predicted Probability of \nMisclassification"
x_lab="Final Interval in Days (centered)"
mm <- sym(vars[1])
xx <- sym(vars[2])
grpColor <- c( CONI="#44AA99",LETE="#AA4499")

model   <- with(dat, glm(as.formula(paste0(resp, modList[[modnum]])),
                         family=binomial,
                         method=regMet
                         ))
grid1 <- fullGridObs
# this function is supposed to take imputations as input, but I don't think it calculates the
# SE correctly. try creating a list of model fits for each of m datasets, and then pooling


intPred <- marginaleffects::predictions(model,
                                        newdata=grid1
)
                
pl <- ggplot(intPred,
       # aes(x=obs_int, y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
       # aes(x=as.numeric(obs_int), y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
       aes(x=obs_int, y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
       # aes(x=!!xx, y=estimate, color=!!mm)) +
       # aes(x=nest_age, y=estimate, color=species)) +
  # geom_point() +
  labs(x=x_lab, y=y_lab)+
  coord_cartesian(ylim=c(0,1))+ 
  scale_color_manual(values=grpColor,
                     labels=c("Common\nNighthawk", "Least Tern"),
                     name="Species") +
  # geom_smooth(method=NULL) +
  geom_line(linewidth=1) +
  geom_ribbon(alpha=0.2)+
  theme_classic() +
  theme(axis.text = element_text(size=18),
  axis.title = element_text(size=20), # can't do margins for x and y axis titles at once
  legend.text=element_text(size=16),
  legend.title=element_text(size=18),
  axis.title.y = element_text(margin=margin(r=5)),
  axis.title.x = element_text(margin=margin(t=5))) 
pl