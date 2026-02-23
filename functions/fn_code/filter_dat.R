

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
                                   "H"   ~ 1,
                                   "F"   ~ 0,
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
                                 "H"  ~ 1,
                                 "F"  ~ 0,
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


