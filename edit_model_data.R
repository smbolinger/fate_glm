#

isU <- function(fate) ifelse(fate %in% c(7:9), 1, 0) # 1 = marked unknown

howMis <- function(fate, c_fate){
  dplyr::case_when(
    fate %in% c(0:6)  & c_fate != fate        ~ "M", # fate    -> a different fate
    fate %in% c(7:9)  & c_fate %in% c(0:6)    ~ "N", # unknown -> assigned a fate
    fate %in% c(0:6)  & c_fate == 7           ~ "U"  # unknown cam fate (field & cam both unknown)
  )
}

########## ADD_VARS ############################################################
add_vars <- function(ndat,  debug = F){
  ndGLM <- ndat %>%
    # could also change this to fate_corr, but it's doing the same thing...
    dplyr::mutate(hatchfail = dplyr::case_when(
      fate %in% c(1:6) ~ 1, # fail
      fate == 0           ~ 0, # hatch
      fate == 7           ~ 7,
      # .data$fate == 8           ~ UHval,
      # .data$fate == 9           ~ UFval,
      is.na(fate)          ~ 7 # MAKE SURE all are coding correctly
    )) %>%

    # why do I not a warning for cfate the way I did for fate?
    dplyr::mutate(c_hatchfail = dplyr::case_when(
      #cfate %in% c(0, 2:6) ~ 0,
      #cfate == 1           ~ 1,
      cfate %in% c(1:6) ~ 1,
      cfate == 0           ~ 0,
      cfate == 7           ~ 7,  # hopefully not many/any of these
      is.na(cfate)         ~ NA # NAs make comparison impossible; better to code as a number?
      # BUT this is a good way to catch them, and the data can't be used anyway
      # is.na(cfate)         ~ 7 # NAs make comparison impossible; better to code as a number?
    )) %>%
    dplyr::mutate(HF_mis = dplyr::case_when(
      # c_hatchfail == 7         ~ NA, # if cfate=u, don't know misclass or not
      c_hatchfail == hatchfail ~ 0,  # nest wasn't misclassified
      c_hatchfail != hatchfail ~ 1   # nest was misclassified
    )) # all the conditions in case_when are mutually exclusive, right?
    ndGLM <- ndGLM %>%
    dplyr::mutate(misclass = dplyr::case_when(
      # cfate == 7 ~ NA,   # camera data inconclusive; unclear whether misclassified
      # fate_corr == cfate ~ 0, # nest wasn't misclassified
      fate == cfate ~ 0, # nest wasn't misclassified
      # fate_corr != cfate ~ 1  # nest was misclassified
      fate != cfate ~ 1  # nest was misclassified
    ) ) %>%
    dplyr::mutate(how_mis =
             dplyr::case_when(misclass==1 ~ howMis(fate, fate),
                       misclass==0 & fate==7 ~ "U",
                       misclass==0 & fate!=7 ~ "C")) %>%
    # dplyr::mutate(is_u = isU(.data$fate_corr))
    dplyr::mutate(is_u = isU(fate))
  return(ndGLM)
  }

########## VAR_TYPES  ############################################################
set_var_types <- function(ndGLM){
  ndGLM <- ndGLM %>%
    dplyr::mutate(
      dplyr::across(c(
        obs_int,
        # fate_date,
        fdate,
        nest_age), as.numeric)) %>%
    dplyr::mutate(
      dplyr::across(c(
        cam_fate,
        species,
        HF_mis,
        misclass,
        how_mis,
        is_u), as.factor)) %>%
    as.data.frame()

  return(ndGLM)

}


########## REFACTOR ############################################################
refactor <- function(ndGLM1, num_fate){
  cat("nest fates before:\n")
  print(table(ndGLM1$cam_fate))
  # 6 FATE CATEGORIES:
  # nestData$cam_fate[nestData$cam_fate == "Ca"] = "Hu"
  ndGLM1$cam_fate[ndGLM1$cam_fate == "Ca"] = "Hu"
  ndGLM1$cam_fate = factor(ndGLM1$cam_fate, levels = c("H", "D", "A", "F", "Hu", "S"))
  # nestData$cfate[nestData$cfate == "Ca"] = "Hu"
  cat("combine Ca with Hu:\n")
  print(table(ndGLM1$cam_fate))

  # 4 FATE CATEGORIES:
  if(num_fate == 4){
    # ndGLM1$c.fate[ndGLM1$c.fate==5] = 3 # 3 becomes the "other" fate category
    # ndGLM1$c.fate[ndGLM1$c.fate==0] = 3
    ndGLM1$cam_fate[ndGLM1$cam_fate == "Hu"] = "F"
    ndGLM1$cam_fate[ndGLM1$cam_fate == "S"] = "F"
    cat("reduce to 4 fates:\n")
    print(table(ndGLM1$cam_fate))
    ndGLM1$cam_fate = factor(ndGLM1$cam_fate, levels = c("H", "D", "A", "F")) # set H as the
    # ndGLM$c.fate = factor(ndGLM$c.fate, levels = c(1, 2, 3, 4))
  }

  # 3 FATE CATEGORIES:
  if(num_fate < 4){ # if num fate == 3?
    ndGLM1$cam_fate[ndGLM1$cam_fate == "Hu"] = "F"
    ndGLM1$cam_fate[ndGLM1$cam_fate == "S"] = "F"
    ndGLM1$cam_fate[ndGLM1$cam_fate == "A"] = "F"
    cat("reduce to 3 fates:\n")
    print(table(ndGLM1$cam_fate))
    ndGLM1$cam_fate = factor(ndGLM1$cam_fate, levels = c("H", "D", "F")) # set H as the
  }

  # PRINT EDITED FATE CATEGORIES:
  cat("reduced to", num_fate, "categories:\n")
  print(table(ndGLM1$cam_fate))
  return(ndGLM1)

}



########## FILTER ############################################################
filter_dat1 <- function(nestData, sites=c("RUTE","RUTW"), spp=c("LETE","CONI"),
                        # cam_nest=TRUE,
                        cam_vect=c("Y","y"),  debug=FALSE, grouped=FALSE){
  # cam_str <- c("Y","y")
  # if (all_cam) cam_str <- c("Y","y","Y*")
  if (debug) cat("camera strings:", cam_vect,"\n\n")
  ndGLM <- nestData %>%

    # mutate(fdate=final_obs_date) %>%    # this is also done later?
    mutate(
      fate = case_match(field_fate,
                                   #"H"   ~ 1,
                                   #"F"   ~ 0,
                                   "H"   ~ 0,
                                   "F"   ~ 1,
                                   "D"   ~ 2,
                                   "S"   ~ 3,
                                   "A"   ~ 4,
                                   "Hu"  ~ 5,
                                   "Ca"  ~ 6,
                                   "U"   ~ 7,
                                   "U?"  ~ 7,
                                   "U-H" ~ 8,
                                   "U-F" ~ 9
           ),

           cfate = case_match(cam_fate,
                                 # "H"  ~ 1,
                                 "H"  ~ 0,
                                 # "F"  ~ 0,
                                 "F"  ~ 1,
                                 "D"  ~ 2,
                                 "S"  ~ 3,
                                 "A"  ~ 4,
                                 "Hu" ~ 5,
                                 "Ca" ~ 6,
                                 "U"  ~ 7
           ),
           camera = ifelse(camera %in% cam_vect, TRUE, FALSE)

    ) %>%

    filter(site %in% sites) %>%
    # filter(species %in% c("CONI", "LETE")) %>%
    filter(species %in% spp) %>%
    filter(camera == TRUE)

  if(grouped==F){
    ndGLM <- ndGLM %>%
    group_by(nest)  %>%
    summarize(status = list(status),
              across(where(is.integer), last), # now they are all chr
              across(where(is.numeric), last),
              across(where(is.character), last) # summarize already goes rowwise
    ) %>%
    rowwise() %>%
    mutate_if(is.list, ~paste(unlist(.), collapse = '|'))

  }

  cat("Make sure you've selected the correct values:\n\n")
  # cat("\nSpecies:\n" )
  print(table(ndGLM$species))
  # cat("\nSite:\n")
  print(table(ndGLM$site))
  # cat("\nYear:\n")
  print(table(ndGLM$year))
  cat("\nHow many NAs are there in camera fate?\n", sum(is.na(ndGLM$cfate)), "\n")
  cat("\nAnd in nest age?\n", sum(is.na(ndGLM$nest_age)), "\n")

  return(ndGLM)
}


uhf_calc <- function(nd, debug=FALSE){
  
  UHFnests <- unlist(nd %>% filter(fate == 8| fate == 9) %>% select(nest))
  
  # view the UH/UF nests:
  if (debug) nd %>% filter(fate==8 | fate==9) %>% select(nest, field_fate, cam_fate, fate, cfate)
 
  # change the fate values based on whether UHisH is TRUE or not: 
  if(UHisH){
    nd$fate[nd$fate==8] = 1
  }else{
    nd$fate[nd$fate==8] = 7
  }
  
  # same with UFisF:
  if(UFisF) nd$fate[nd$fate==9] = 0 else nd$fate[nd$fate==9] = 7
  
  # view the same nests again:
  # nd %>% filter(nest %in% UHnests) %>% select(nest, field_fate, cam_fate, fate, cfate)
  if(debug) nd %>% filter(nest %in% UHFnests) %>% select(nest, field_fate, cam_fate, fate, cfate)
  return(nd)
}


