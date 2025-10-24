

# this is for question 1:
# 
# modnum <- 4
modnum <- 10
# dat <- impDat
dat <- impInt
# vars <- c("species", "nest_age")
vars <- c("species", "obs_int")
y_lab="Predicted Probability of \nUnknown Field Fate"
# x_lab="Nest Age in Days (centered)"
x_lab="Final Interval in Days (centered)"
mm <- sym(vars[1])
xx <- sym(vars[2])
grpColor <- c( CONI="#44AA99",LETE="#AA4499")
 
model   <- with(dat,
                glm(as.formula(paste0(resp, modList[[modnum]])),
                family=binomial,
                method=regMet
                ))
# intPred <- 
grid1 <- fullGridObs
intPred <- marginaleffects::predictions(model, newdata=grid1)
# intPred <- marginaleffects::predictions(model, type="link", newdata=grid1)
pl <- ggplot(intPred,
       # aes(x=obs_int, y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
       # aes(x=as.numeric(obs_int), y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
       aes(x=obs_int, y=estimate, ymin=conf.low, ymax=conf.high, color=species)) +
       # aes(x=!!xx, y=estimate, color=!!mm)) +
       # aes(x=nest_age, y=estimate, color=species)) +
  # geom_point() +
  labs(x=x_lab, y=y_lab)+
  # ylim(0,1)+ # using ylim by itself modifies the underlying data
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
#  pl3 <- intPlot(impDat,                             # (interaction between species & nest_age)
#           # modnum=5)
#           modnum=4,
#           vars = c("species", "obs_int"), # in this case, intVar1 should always be species
#           # grid1=gr,
#           grid1=intGridObs,
#           x_lab="Final interval - centered (days)",
#           y_lab="Predicted probability of\nunknown field fate")
#   # ,
#           # y_lab="Predicted probability of\n unknown field fate")                   
# # }
# pl3

pl

ggsave(filename=paste("interaction_plot", quest, resp, ".svg", sep="_"), plot=pl, device="svg", )