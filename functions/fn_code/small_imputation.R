
# Also tells R to use the smaller set for the other functions

# impute, without completing the datasets so you can use the fit command?
imp_small <- function(){
  impDat_small <- imputeDat( impDat1 = imppDat, return= "") # default m value is 10
  smallFit <- impFit(impDat_small, modList)
  smallPool <- poolImp(smallFit, modList)
  # Error in `brglm2::brglm_fit`(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  : 
  #  could not find function "brglm2::brglm_fit"
  #       MAYBE HELPFUL:
  # pkg::name returns the value of the exported variable name in namespace pkg, whereas pkg:::name returns the value of the internal variable name. 
  
  # impDatsmall  <- mice::mice(impDat1, seed=613, m=10, printFlag=FALSE)
  # impDatsmall2 <- mice::complete(impDatsmall, "all") # generate a list of all the completed datasets
  impDat <- impDat_small
  impMod <- smallFit
  impModPool <- smallPool
}