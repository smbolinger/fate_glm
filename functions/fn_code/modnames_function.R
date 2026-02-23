modnames <- function(
    mods, null=TRUE, debug=FALSE
){
  dp <- function(x) deparse(x, width.cutoff=150)
  # message(paste("the model call:", sapply(mods, function(x) stringr::str_extract(dp(x$call)))))
  if(is.list(mods)){
    modEQ <- paste(sapply(
      mods,
      # the model calls have changed, so this doesn't work anymore:
      # function(x) stringr::str_extract(dp(x$call), "(?<=~\\s)(.*)(?=\")")
      # need to use formula
      # but I still can't figure out why the regex stopped working...
      function(x) stringr::str_extract(dp(x$formula), "(?<=~\\s).*")
      # function(x) stringr::str_extract(x$formula, "(?<=~\\s)(.*)  ")
    ), sep = ",")
  } else {
  modEQ <- stringr::str_extract(mods, "(?<=~\\s).*")
  }
  # modEQ <- paste(sapply(modList, function(x) stringr::str_extract(modList, "(?<=~\\s).*")), sep=",")
  # modEQ1
  # modEQ
  modEQ <- stringr::str_replace_all(modEQ, c(
    "species" = "SPECIES",
    "obs_int" = "FINAL_INTERVAL",
    "cam_fate" = "TRUE_FATE",
    "nest_age" = "NEST_AGE",
    "fdate" = "END_DATE",
    "1"     = "NULL"))
  # cat("mod equations before substitution:", modEQ)
  if(debug) message(paste("\nmod equations before substitution: ", modEQ))
  # print(con=stdout(), "mod equations before substitution:", modEQ)
  m <- stringr::str_match(modEQ, "(\\w+)\\s\\*\\s(\\w+)" ) # output in matrix form
  # m
  # don't want to replace wheree this is no match (just replaces with NA)
  # modEQ <- stringr::str_replace(modEQ[!is.na(m)], coll(m[,1]), coll((paste0(m[,2]," + ",m[,3]," + ",m[,2],":",m[,3]))))
  modEQ <- ifelse(!is.na(m[,1]),
                  str_replace(modEQ, coll(m[,1]), paste0(m[,2]," + ",m[,3]," + ", m[,2],":",m[,3])),
                  modEQ
  )
  # if(debug) message(paste("\nand again after str_match", m))
  if(debug) message(paste("\nand again after str_match/str_replace:", modEQ))
  # n <- gsub(" ", "\\\\s", str_match(modEQ, "(\\w+)\\s\\*\\s(\\w+)") )
  # n
  
  # I think all of this was because the variables were not always in the same order...
  # p <- paste(m[,1], sep="|", collapse="|") # this grabs the correct strings
  # p <- gsub(" ", "\\\\s", p)
  # p <- gsub("\\*", "\\\\*", p)
  #
  #   r <- function(eq) {
  #     # eq = "species * obs_int"
  #     r = which(m[,1] %in% eq)
  #     paste0(m[r,2]," + ",m[r,3]," + ",m[r,2],":",m[r,3])
  #   }
  #
  #   modEQ <- stringr::str_replace(modEQ, p, r)
  #   if (null==TRUE){
  #     modEQ[2] <- "NULL"
  #   }
  
  return(modEQ)
}


modnames(mods)
modnames(modList)
