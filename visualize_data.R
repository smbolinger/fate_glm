

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



#' Create frequency table and save to file
#'
#' Creates a separate frequency table for each response variable and then saves them as .rtf files
#'
#' @param ndata must include the variables cam_fate, species (categorical predictor vars) and HF_mis, misclass, & is_u (response vars)
#' @param vars response variables to include, with emphasis on misclasification (MC) variable choice. options are "all" (default, use both MC variables), "misclass", or "HF_mis"
#' @param suffix - for filename when saving. default is ""
#' @param debug - default FALSE
#' @return the filename of the saved frequency table, which can be used in knitr
#' @export
#'
#' @importFrom magrittr %>%
# fr_tab <- function(ndata, vars=c("HF_mis","misclass","is_u")){
fr_tab <- function(ndata, vars="all", suffix="", debug=F){

  pre1 <- ndata |>
    dplyr::mutate(HF_mis = .data$HF_mis == 1) %>%                            # response var # 1
    gtsummary::tbl_summary(by=.data$HF_mis,
                include=c(.data$species, .data$cam_fate),
                type=list(where(is.logical) ~ "categorical"), #doesn't work
                label=list(.data$species ~ "Species",
                           .data$cam_fate ~ "Camera fate"),
                statistic=list(all_categorical()~"{n}")) %>%
    gtsummary::modify_header(label ~ "**Variable**",
                  all_stat_cols() ~ "**{level}**<br>(N={n})") %>%

    gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Nest misclassified (H/F)**") |>
    gtsummary::bold_labels()

  pre2 <- ndata %>%
    dplyr::mutate(misclass = .data$misclass == 1) %>%                       # response var # 2
    gtsummary::tbl_summary(by=.data$misclass,
                include=c(.data$species, .data$cam_fate),
                type=list(where(is.logical) ~ "categorical"), #doesn't work
                label=list(.data$species ~ "Species",
                           .data$cam_fate ~ "Camera fate"),
                statistic=list(all_categorical()~"{n}")) %>%
    gtsummary::modify_header(label ~ "**Variable**",
                  all_stat_cols() ~ "**{level}**<br>(N={n})") %>%
    gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Nest misclassified (all fates)**") %>%
    gtsummary::bold_labels()

  pre3 <- ndata |>
    dplyr::mutate(is_u = .data$is_u == 1) %>%                                # response var # 3
    gtsummary::tbl_summary(by=.data$is_u,
                include=c(.data$species, .data$cam_fate),
                label=list(.data$species ~ "Species",
                           .data$cam_fate ~ "Camera fate"),
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

  } else print("invalid argument - vars")

  pr <- pr %>%
    gtsummary::as_gt() %>%
    gt::tab_options(table.width = gt::pct(40))
  now = format(Sys.time(), "%m%d_%H%M_")
  filename <- ifelse(
    "2021" %in% ndata$year,
    sprintf("frtab_inc2021_%s.png", now),
    sprintf("frtab_no2021_%s.png", now)
  )
  filename2 <- sprintf("fr_tab_%s_%s.rtf", now, suffix)
  pr %>% gt::gtsave(filename=filename, path="analysis/", vwidth=1200, vheight=800)
  pr %>% gt::gtsave(filename=filename2, path="analysis/", vwidth=1200, vheight=800)

  return(filename)
}



#' Plot the (numeric) predictor variables in a histogram
#'
#'
#' @param resp response variable
#' @param dat  nest data, in a dataframe
#' @param vars  vector containing the names of the predictors
#' @param filTit legend title
#' @param filLabs legend labels
#' @param filColors variable level colors
#' @param xLabs x axis labels, in same order as the predictors vector
#' @param yLab y axis label (count)
#' @param legendPos change the position of the legend; default is "right"
#' @param xMaxVect xmax values, in same order as predictors
#' @param binWidthVect bin width values (if applicable), in same order as predictors
#' @param nBins number of bins for the histogram; default is 6. Will be ignored if bin width is specified.
#' @param suffix for the save file; default is empty
#' @param dir directory for the save file; default is figures
#' @param brVect list of axis breaks, with a list for each variable (default is blank list)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang sym, .data
#' @returns a combined plot of all the predictors
#' @export
#'
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
                     suffix="", dir="figures"){
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
  now = format(Sys.time(), "_%m%d_%H%M_")
  # datName <- deparse(substitute(dat))
  # datName <- rlang::as_label(enexpr(!!dat))
  # fname <- paste0("catplot_",resp,"-",datName,now,suffix)
  fname <- paste0(dir,"/catplot_",resp,now,suffix,".svg")
  # ggsave("catplot_isu_NAremoved.svg",
  ggplot2::ggsave(fname,
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
#' @param ndGLM nest data - dataframe
#'
#' @returns nothing
#' @export
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