

#' Visually inspect the data

#' @param nestData - data table
#'
#' @importFrom magrittr %>%
#' @export

inspect_dat <- function(nestData){
    # could add a vars argument
    cat("visually check for outliers in:")
    plot(nestData$nest_age, main="Nest Age")
    plot(nestData$fate_date, main="Fate Date")
    plot(nestData$obs_int, main="Final Obs Interval")


    cormat <- nestData %>%
        dplyr::select(.data$nest_age, .data$obs_int, .data$fdate) %>%
        data.matrix() %>%
        PerformanceAnalytics::chart.Correlation(histogram=T)

    cat("any evidence of correlation between variables?")
    cormat
    # dataM = ndGLM2
    # cam_sep <- glm(is_u ~ species + obs_int * cam_fate + nest_age,
    #                data=dataM,
    #                family=binomial,
    #                method=detectseparation::detect_separation)
    # cat("is there evidence of separation?\n", cam_sep)
}

print_vars <- function(ndGLM2){
    cat("Camera fates:\n", table(ndGLM2$cam_fate))
    cat("\nSpecies:\n", table(ndGLM2$species))
    cat("\nFinal observation intervals:\n", table(ndGLM2$obs_int))
    cat("\nDate fate was assigned:\n", table(ndGLM2$fate_date))
    cat("\nNest age when fate assigned:\n", table(ndGLM2$nest_age))
}


print_stuff <- function(ndGLM2){
    cat("WITH 4 FATE CATEGORIES:\n")
    cat("Camera fates:\n")
    table(ndGLM2$cam_fate)

    cat("How many we said hatched:")
    table(ndGLM2$hatchfail)

    cat("How many actually hatched:")
    table(ndGLM2$c_hatchfail)

    cat("Number of nests misclassified (hatch or fail:")
    table(ndGLM2$HF_mis)

    cat("Number of failed nests misclassified:")
    table(ndGLM2$misclass[ndGLM2$final_fate %in% c(0, 2:6) ])

    cat("Number of unknown nests newly classified:")
    table(ndGLM2$misclass[ndGLM2$final_fate %in% c(7,8)])

    hfMis <- ndGLM2[ndGLM2$HF_mis==1,] # is this supposed to be ndGLM or ndGLM2?
# cat("\nMisclassified nests by camera (true) fate:\n")
# table(hfMis$c_hatchfail)

    cat("\n\nUnknown nests with true fate hatch vs fail:\n")
    unk <- ndGLM2[ndGLM2$final_fate==7,]
    table(unk$c_hatchfail)

    cat("\n\nFailed nests where cause of failure was wrong:\n")
    f <- ndGLM2[ndGLM2$c.fate %in% c(0, 2:6) & ndGLM2$final_fate %in% c(0, 2:6),]
    table(f$misclass)

    cat("\n\nMisclassified nests by species:\n")
    m <- ndGLM2[ndGLM2$misclass==1,]
    table(m$species)

    cat("\n\nH/F misclassified by species:")
    hm <- ndGLM2[ndGLM2$HF_mis==1,]
    table(hm$species)
}

# fr_tab <- function(ndata, vars=c("HF_mis","misclass","is_u")){
#mk_fr_tab <- function(ndata, homeDir, save=TRUE, vars="all", suffix="", debug=F){
mk_fr_tab <- function(ndata, save=TRUE, vars="all", suffix="", debug=F){

    pre1 <- ndata |>
        dplyr::mutate(HF_mis = HF_mis == 1) %>%                            # response var # 1
        gtsummary::tbl_summary(by=HF_mis,
                               include=c(species, cam_fate),
                               type=list(where(is.logical) ~ "categorical"), #doesn't work
                               label=list(species ~ "Species",
                                          cam_fate ~ "Camera fate"),
                               statistic=list(all_categorical()~"{n}")) %>%
        gtsummary::modify_header(label ~ "**Variable**",
                                 all_stat_cols() ~ "**{level}**<br>(N={n})") %>%

        gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Nest misclassified (H/F)**") |>
        gtsummary::bold_labels()

    #pre2 <- ndata %>%
    #    dplyr::mutate(misclass = misclass == 1) %>%                       # response var # 2
    #    gtsummary::tbl_summary(by=misclass,
    #                           include=c(species, cam_fate),
    #                           type=list(where(is.logical) ~ "categorical"), #doesn't work
    #                           label=list(species ~ "Species",
    #                                      cam_fate ~ "Camera fate"),
    #                           statistic=list(all_categorical()~"{n}")) %>%
    #    gtsummary::modify_header(label ~ "**Variable**",
    #                             all_stat_cols() ~ "**{level}**<br>(N={n})") %>%
    #    gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Nest misclassified (all fates)**") %>%
    #    gtsummary::bold_labels()

    pre3 <- ndata |>
        dplyr::mutate(is_u = is_u == 1) %>%                                # response var # 3
        gtsummary::tbl_summary(by=is_u,
                               include=c(species, cam_fate),
                               label=list(species ~ "Species",
                                          cam_fate ~ "Camera fate"),
                               statistic=list(all_categorical()~"{n}")) %>%

        gtsummary::modify_header(label ~ "**Variable**",
                                 all_stat_cols() ~ "**{level}**<br>(N={n})") %>%
        gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Marked unknown in field**") %>%
        gtsummary::bold_labels()
    if(vars=="all"){
        pr <- gtsummary::tbl_merge(tbls=list(pre1, pre2, pre3),
                                   tab_spanner = c("**Misclassified (H/F)**",
                                                   "**Misclassified**",
                                                   "**Marked unknown**"))
    } else if(vars=="misclass"){
        pr <- gtsummary::tbl_merge(tbls=list(pre3, pre2),

                                   tab_spanner = c(  "**Marked unknown**",
                                                   "**Misclassified**"))

    } else if(vars=="HF_mis"){
        pr <- gtsummary::tbl_merge(tbls=list(pre3,pre1 ),
                                   tab_spanner = c("**Marked unknown**",
                                                   "**Misclassified (H/F)**"))

    } else {
        print("invalid argument - vars")
    }

    if(debug) cat("\no o o FREQUENCY TABLE: o o o \n")
    if(debug)  qvcalc::indentPrint(as_tibble(pr))

    dummy_var <- function(mat, debb=debug){
    #dummy_var <- function(mat, debug=TRUE){
        if (debb) cat("\ndimensions of matrix:", dim(mat))
        rownames(mat) <- seq(1,nrow(mat)) # maybe need to set rownames before picking out 2:3?
        rownames(mat)[2:3] <- paste0("species", mat[2:3,1])
        rownames(mat)[5:10] <- paste0("cam_fate", mat[5:10,1])
        mat <- mat[-c(1,4),-1]
        rnames <- rownames(mat)
        if(debb) cat("\trownames:", rnames)
        if(debb) cat("\tlength of rownames:", length(rnames), length(rownames(mat)))
        mat <- apply(mat, 2, as.numeric)
        rownames(mat) <- rnames
        colnames(mat) <- c("no", "yes")
        return(mat)
     }

    fr_tab <- pr %>%
        gtsummary::as_gt() %>%
        gt::tab_options(table.width = gt::pct(40))

    fr_mat1 <- pre1 %>% 
        as_tibble() %>% 
        as.matrix() %>%
        dummy_var()
     if(debug) cat("\n+++ frequencies for q1 - dimnsions: ", dim(fr_mat1), "+++ and matrix: \n")
     if(debug) qvcalc::indentPrint(fr_mat1)

    fr_mat2 <- pre3 %>% 
        as_tibble() %>% 
        as.matrix() %>%
        dummy_var()
     #fr_mat2 <- dummy_var(fr_mat2)
     if(debug) cat("\n+++ frequencies for q2 - dimnsions: ", dim(fr_mat2), "+++ and matrix: \n")
     if(debug) qvcalc::indentPrint(fr_mat2)

    # outd <- paste0(homeDir,"/out/")
    # can't just pass the name or it can't find the table itself?
    # filename <- save_tab("fr_tab1",outdir=outd, suffix = suffix,rtf = rtfOn)
    if (save){
        outd <- paste0(homeDir,"/tables/")
        filename <- save_tab(fr_tab,outdir=outd, suffix = suffix,rtf = rtfOn)
        return(filename)
    }
    # now = format(Sys.time(), "%m%d_%H%M_")
    # filename <- ifelse(
    #   "2021" %in% year,
    #   sprintf("frtab_inc2021_%s.png", now),
    #   sprintf("frtab_no2021_%s.png", now)
    # )
    # filename2 <- sprintf("fr_tab_%s_%s.rtf", now, suffix)
    # filename2 <- make_fname("fr_tab", suffj)
    # pr %>% gt::gtsave(filename=filename, path="analysis/", vwidth=1200, vheight=800)
    # pr %>% gt::gtsave(filename=filename2, path="analysis/", vwidth=1200, vheight=800)
    return(list(fr_mat1, fr_mat2))
}

plot_predictors <- function(resp, dat, vars,
                     filTit="", filLabs, filColors,
                     xLabs, yLab, legendPos="right",
                     # xMaxVect, binWidthVect=NA, nBins=6){
                     brVect = list(var1=list(),
                                   var2=list(),
                                   var3=list()),
                     # brVect = list(var1=NULL,
                     #               var2=NULL,
                     #               var3=NULL,
                     xMaxVect, binWidthVect=c(0,0,0), nBins=6,
                     suffix="", dir="/figures/"){
  # numBreaks=5){
# maybe making this needlessly complicated....
# but I can make a more general plotting function later
  if(resp %in% c("is_u","HF_mis")){
    dat <- dat %>%
      # mutate(across(c(nest_age, fdate, obs_int), as.integer)) %>%
      dplyr::mutate(fillVal = !(.data[[resp]]==0)) # switch the values so 0=TRUE
  } else {
    dat <- dat %>%
      dplyr::mutate(fillVal = .data[[resp]])
  }
  pl <- list()
  for(v in seq_along(vars)){

    # if(is.na(binWidthVect[1])){
    # if(binWidthVect==c(0,0,0)){
    if(sum(binWidthVect) == 0){
      numBins = nBins
    }else{
      numBins = round(xMaxVect[v]/binWidthVect[v])
    }

    print(vars[v])
    if(length(brVect[[v]] > 0)){
      br = unlist(brVect[[v]])
    } else {
      br=ggplot2::waiver()
    }
    # br <- seq(0, xMaxVect[v], by=binWidthVect[v])
    pl[[v]] <- ggplot2::ggplot(dat, ggplot2::aes(x=!!sym(vars[v]))) +
      ggplot2::geom_histogram(ggplot2::aes(fill=.data$fillVal),
                     # bins=max(dat$obs_int)) +
                     bins=numBins) +
      # binwidth=binWidthVect[v]) +
      # theme_classic() +
      # ylim(0,80) +
      ggplot2::scale_x_continuous(breaks=br) +
      ggplot2::scale_fill_discrete(name=filTit,
                          labels=filLabs,
                          type=filColors) +

      ggplot2::labs(y=yLab, x=xLabs[v]) +
      ggplot2::theme_classic()
  }

  combPl <- patchwork::wrap_plots(pl,
                                  ncol  =length(pl),
                                  axes  ="collect" # remove duplicate axes
  ) +
    patchwork::plot_layout(guides="collect") &
    ggplot2::theme(axis.text            = ggplot2::element_text(size=14),
          axis.title.y         = ggplot2::element_text(size=16,
                                              margin=ggplot2::margin(r=12)),
          axis.title.x         = ggplot2::element_text(size=16,
                                              margin=ggplot2::margin(t=12)),
          # legend.position="top",
          legend.position=legendPos,
          legend.text          = ggplot2::element_text(size=12),
          legend.title         = ggplot2::element_text(size=14),
          # legend.frame         = element_rect(linewidth=1),
          legend.box.background = ggplot2::element_rect(color="black"),
          legend.key.spacing.y = ggplot2::unit(0.2, "cm"))



  print(combPl)
  # now = format(Sys.time(), "_%m%d_%H%M_")
  now = format(Sys.time(), "_%m%d_")
  # datName <- deparse(substitute(dat))
  # datName <- rlang::as_label(enexpr(!!dat))
  # fname <- paste0("catplot_",resp,"-",datName,now,suffix)
  # fname <- paste0(dir,"/catplot_",resp,now,suffix,".svg")
  fname <- paste0("catplot_",resp,suffix,now,".svg")
  # outd <- paste0(homeDir, dir)
  # paste(homeDir, dir)
  # ggsave("catplot_isu_NAremoved.svg",
  ggplot2::ggsave(fname,
                  # path = paste0(homeDir,dir),
                  path = paste0(homeDir,dir),
                  # plot=catPlotU,
                  plot= combPl,
                  device="svg",
                  width=24,
                  height=8,
                  units="cm"
                  )
  # return(combPl)
  return(fname)
}



#' Print the response variables that were added
#'
print_vars <- function(ndGLM){
  cat("\nhatchfail variable:\n")
  print(table(ndGLM$hatchfail, useNA = "ifany"))
  cat("\nc_hatchfail variable:\n")
  print(table(ndGLM$c_hatchfail, useNA = "ifany"))
  cat("\nHF_mis variable:\n")
  print(table(ndGLM$HF_mis, useNA = "ifany"))
  cat("\nmisclass variable:")
  print(table(ndGLM$misclass, useNA = "ifany"))
  cat("\nhow_mis variable:")
  print(table(ndGLM$how_mis, useNA = "ifany"))
  cat("\nis_u variable:")
  print(table(ndGLM$is_u, useNA = "ifany"))
}

suppress_warnings <- function(.expr, .f, ...) {
  eval.parent(substitute(
    withCallingHandlers( .expr, warning = function(w) {
      cm <- conditionMessage(w)
      cond <- 
        if(is.character(.f)) grepl(.f, cm) else rlang::as_function(.f)(cm,...)
      if (cond) {
        invokeRestart("muffleWarning")
      }
    })
  ))
}

############## FILE NAMES ################################################
# make_fname <- function(dir, name, suffix, extension, nowDigits=6){
make_fname <- function( name, suffix, extension, nowDigits="long"){
  if(nowDigits=="long") {
    now = format(Sys.time(), "%m%d_%H%M")
  } else if (nowDigits=="short") {
    now = format(Sys.time(), "%m%d")
  } else {
    now = ""
  }
  # fname <- paste0(name, suffix, now, extension)
  fname <- paste0(paste(name, suffix, now, sep="_"), extension)
  return(fname)
}
############## SAVE TABLES ###############################################
save_tab <- function(tab, dpi=(1800/6), outdir, suffix="", rtf=FALSE){

  # now = format(Sys.time(), "_%m%d_%H%M%S") # seconds bc tables all generated in < 1 min
  nm  <- function(x) deparse(substitute(x)) 
  # tab <- get(tabName)
  # dpi      <- (1800/6)                          # img width (px) / desired img width (in)
  if(rtf){
    # file_name <- paste0(nm(tab), suffix, now, ".rtf") # name of tab will always be tab within the function?
    # file_name <- make_fname(tabName, suffix, ".rtf", nowDigits="short")
    file_name <- make_fname(nm(tab), suffix, ".rtf", nowDigits="short")
    # gt::gtsave(tab, file_name, path=paste0(dir,"out/"))
    gt::gtsave(tab, file_name, path=paste0(outdir,"tables/"))
  }

  # file_name1 <- paste0(nm(tab), now, suffix, ".png")
  file_name1 <- make_fname(nm(tab), suffix, ".png", nowDigits="short")
  # gt::gtsave(tab, file_name1, path=paste0(dir,"out/"))
  gt::gtsave(tab, file_name1, path=paste0(outdir,"tables/"))
  

  # file_name2 <- paste0(nm(tab), now, "_rounded.png")
  # tab2 <- tab %>% fmt_number(decimals=2)
  # gtsave(tab2, file_name2, path-"analysis/")
  fn <- paste0(outdir,"tables/",file_name1)
  # return(file_name1) # return name of png to add to knit output
  return(fn) # return name of png to add to knit output
}

# 
# withCallingHandlers({
#   x <- 0
#   warning("Unrecognized record 123")
#   x <- x + 1
#   warning("another warning")
#   x + 1
# }, warning = function(w) {
#   if (startsWith(conditionMessage(w), "Unrecognized record"))
#     invokeRestart("muffleWarning")
# })

if (FALSE){
  suppress_warnings({sqrt(-1); warning("ooops", call. = FALSE)}, startsWith, "o")
  sqrt(-1)
  # Warning message:
  # In sqrt(-1) : NaNs produced
  suppress_warnings({sqrt(-1); warning("ooops", call. = FALSE)}, ~nchar(.)>10)
  # Warning message:
  # ooops
  suppress_warnings({sqrt(-1); warning("ooops", call. = FALSE)}, "NaN")
  # Warning message:
  # ooops
}

## From: https://stackoverflow.com/questions/16517795/selective-suppresswarnings-that-filters-by-regular-expression/16521046#16521046
