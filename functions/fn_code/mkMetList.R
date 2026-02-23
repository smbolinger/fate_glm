
########################################################################################
####### MAKE METHOD LIST #############################################################
########################################################################################

if(FALSE){
    fateMet <- "pmm"
    ageMet <- "pmm"
    misMet <- "pmm"
    resp <- "HF_mis"
    col_sel[6] <- "HF_mis"
    col_sel <- c(col_sel, "speciesLETE:is_u")
    col_sel <- names(ampDat)
    c<-col_sel[2]
    c <- inter
    met <- "rf"
    met <- "caliber"
    met <- "passive"
    met <- "stratify"
    dat <- ampDat
    int <- NULL
    int <- paste(inters, collapse="*")
}
# mkMetList <- function(met, dat, col_sel, int=NULL, debug=FALSE){
mkMetList <- function(met, dat, int=NULL, debug=FALSE){
    col_sel <- names(dat)
    metList <- rep("",length(col_sel))
    catList <- c("HF_mis", "cam_fate", "species", "is_u")
    names(metList) <- col_sel

    colNA <- names(dat)[colSums(is.na(dat)) > 0]
    is_num <- names(dat)[sapply(dat, is.numeric)]
    is_bin <- names(dat)[sapply(dat, function(x) length(levels(x)))==2]
    is_cat <- names(dat)[sapply(dat, function(x) length(levels(x)))>2]


    for(c in col_sel){
    if(met=="caliber"){
    if(c %in% catList ) met1 = "rfcat" else met1 = "rfcont"
    }else if(met %in% c("default.int", "pmm.int")){
    met1 <- str_extract(met, pattern = "\\w+(?=.)") # everything up until the period
    # }else if(met == "passive.int"){
    }else if(met == "passive" | met=="stratify" | met=="cf_cc"){
    # met1 <- ""
    # met1 <- ifelse(is.numeric())
    met1 <- case_when(c %in% is_num ~ "pmm",
            c %in% is_bin ~ "logreg",
            # c %in% is_cat ~ "polyreg")
            c %in% is_cat ~ "pmm") # because I have many cells w/ <10 obs
    # met2 <- paste("~I(", int,")")
    } else {met1 <- met}

    interTrue <- ifelse((grepl(".",c,fixed=T)), TRUE, FALSE)
    # make NAs so that those columns can be dropped later
    metList[c] <- case_when(
    # interTrue & met=="passive" ~ paste("~I(",int,")"),
    met=="cf_cc" & c=="cam_fate" ~ "",
    met =="default" ~ NA,
    met == "passive" ~ NA,
    met=="stratify" & c=="species" ~ NA, # this should still exist, but take species out of formula?
    # met =="default" ~ "",
    interTrue & met !="passive" ~ NA,
    c=="inter" ~ NA,
    !interTrue & c%in%colNA ~ met1,
    .default = ""
    )

    }
    return(metList)
}
