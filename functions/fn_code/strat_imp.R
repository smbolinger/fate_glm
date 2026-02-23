
# impStrat <- function(datNameImp, met, col_sel){
impStrat <- function(datNameImp, met){
  col_sel <- c(prVars, resp) # columns to select, as strings
  datLT <- get(datNameImp) %>% filter(species=="LETE") %>% select(all_of(col_sel))
  impLT <- mice::mice(datLT, seed=613, m=30, method="pmm", printFlag=FALSE)
  
  datCO <- get(datNameImp) %>% filter(species=="CONI") %>% select(all_of(col_sel))
  impCO <- mice::mice(datCO, seed=613, m=30,method="pmm", printFlag=FALSE)
  
  # impInt <- mice::cbind(impLT, impCO)
  impIntPMM <- mice::rbind(impLT, impCO)

  # imp_plot(impInt, plType = "density")
  # imp_plot(impInt, nest_age ~ fdate, plType="xy")
}