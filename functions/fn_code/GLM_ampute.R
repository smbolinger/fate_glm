
## all of this doesn't work because you don't know which fate it is if it's missing:
## 
# dat4imp <- ndGLM_scl
# mice::md.pattern(dat4imp, rotate.names = TRUE)
# dummyvars <- model.matrix( ~ cam_fate -1, data=dat4imp)
# dummyvars
# dat4imp$speciesLETE <- ifelse(dat4imp$species=="LETE", 1, 0)
# dat4imp$speciesCONI <- ifelse(dat4imp$species=="CONI", 1, 0)
# dat4imp <- dat4imp[, c("obs_int", "nest_age", "fdate", "HF_mis", "is_u", "speciesLETE", "speciesCONI")]
# dat4imp <- cbind(dat4imp, dummyvars)
# dat4imp
# mice::md.pattern(dat4imp, rotate.names = TRUE)
# mice::md.pairs(dat4imp)
# 
# ampPlList <- list()
# for(v in names(dummyvars)){
#   ampPlList[v] <- naniar::gg_miss_var()
# }
source("missingness_tab.R")


dat4amp <- ndGLM_scl_cc
add_dummy <- function(dat){
  dummy_vars <- model.matrix(~ cam_fate -1, data = dat)
  dummy_vars
  
  dat$speciesLETE <- ifelse(dat$species=="LETE", 1, 0)
  dat$speciesCONI <- ifelse(dat$species=="CONI", 1, 0)
  
  dat<- dat[, c("obs_int", "nest_age", "fdate", "HF_mis", "is_u", "speciesLETE", "speciesCONI")]
  # dat4amp <- cbind(dat4amp, dummy_vars, speciesLETE, speciesCONI)
  dat <- cbind(dat, dummy_vars)
  return(dat)
}

dat4amp <- add_dummy(dat4amp)
cat("add dummy variables and remove factors. new columns:\n", names(dat4amp))

amp_out <- mice::ampute(dat4amp)
cat("\n\nCreate missing values, default settings:\n")
print(mice::md.pattern(amp_out$amp, rotate.names = TRUE))

# out$prop
# new_prop <- 0.15
new_patt <- amp_out$patterns
# new_patt

no_miss <- names(new_patt)[c(1, 3, 5:7)]
# new_name
# new_patt[,c("cam_fateH","cam_fateD", "cam_fateA", "cam_fateHu", "cam_fateS", "cam_fateF", "speciesCONI", "speciesLETE", )] <- 1
# new_patt[no_miss] <- 1
# new_patt
# is_miss <- new_patt[,!no_miss]
# is_miss <- new_patt[,-c(no_miss)]
is_miss <- new_patt[,-c(1,3,5:7)]

# This creates waaay too many patterns...
# miss_seq <- rep(list(0:1), 8)
# miss_patt <- expand.grid(miss_seq)
# miss_patt

# HF_mis will always be missing when cam_fate is missing
miss_pat <- list(
# miss_pat <- data.frame(
miss_age <- c(0, rep(1,7)), 
# miss_mis <- c(1,0, rep(1,6)),
# miss_fate<- c(1,1,rep(0,6)),
# miss_am  <- c(0,0,rep(1,6)),
miss_fm  <- c(1,rep(0,7)),
# miss_af  <- c(0,1,rep(0,6)),
miss_afm <- c(rep(0,8))
# miss_none<- c(rep(1,8))
)
m <- do.call(rbind,miss_pat) # create the matrix of the missingness patterns 
# names(m) <- names(is_miss) # not sure matrices can have names
# rep(c(rep(1,5)),8)
# rep(c(rep(1,5)),7)
rep(c(rep(1,5)),3)
# p <- do.call(rbind, rep(c(rep(1,5)), 8)) # create additional columns for the vars not missing values
p <- do.call(rbind, rep(list(c(rep(1,5))), 3)) # create additional columns for the vars not missing values

new_prop <- 0.2
miss_patt_mat <- cbind(m,p)
# patt_freq <- c(0.25,0.25,0.25,rep(0.05, 5))
# patt_freq <- c(0.3,0.3,0.3,rep(0.025, 4))
patt_freq <- c(0.45,0.45,0.1)

# cat("\nmissingness matrix:", miss_patt_mat)
cat("\n>> missingness matrix:\n")
print(miss_patt_mat)
cat("\n>> proportion missing:", new_prop)
cat("\n>> frequency of each pattern:", patt_freq)

new_order <- c(names(is_miss), no_miss)
cat("\n>> reorder columns:", new_order)
# new_order
dat4amp <- dat4amp %>% select(new_order) # reorder the columns to match the matrix


# out <- mice::ampute(dat4amp, prop = 0.2, patterns = miss_patt_mat, freq = patt_freq)
amp_out <- mice::ampute(dat4amp, prop = new_prop, patterns = miss_patt_mat, freq = patt_freq)
cat("\n\nCreate new missing values:\n")
print(mice::md.pattern(amp_out$amp, rotate.names = TRUE))
print(missing_tab("amp_out", prVars))


wts <- amp_out$weights
wts[1,] <- c(0, 1, 0, 0.8, 0.2, rep(0,8))
cat("\n>> new weights:\n")
print(wts)

amp_out_wt <- mice::ampute(dat4amp, prop = new_prop, patterns = miss_patt_mat, freq = patt_freq,weights = wts)
cat("\n\nCreate more new missing values, with weighted probabilities:\n")
print(mice::md.pattern(amp_out_wt$amp, rotate.names = TRUE))
missing_tab("amp_out_wt", prVars)
# m <- as.data.frame(miss_pat)
# m <- data.table::rbindlist(miss_pat)
# m <- as.matrix(miss_pat)
# miss_pat
# freq_vec <- rep(0, 256)
# freq_vec[255] <- 0.45
# freq_vec[]

if(FALSE){
  out2 <- mice::ampute(dat4amp, prop=0.15, patterns = miss_patt)
  
  dat4amp2 <- ndGLM_scl
  dat4amp2 <- add_dummy(dat4amp2)
  colSums(is.na(dat4amp2))
  out_2 <- mice::ampute(dat4amp2)
}