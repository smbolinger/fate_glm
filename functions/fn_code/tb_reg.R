
  tabOR <- gtsummary::tbl_regression(mods[[modNum]],
                          label = vnames,
                          exponentiate=T)
  headr <- paste("Regression Summary for Model:",
                 modnames(list(x = mods[[modNum]]), null=FALSE),
                 sep="\n")


  now = format(Sys.time(), "%m%d_%H%M_")
  filename3  <- sprintf("reg_%s_%s.rtf", now, suffix)
  filename4  <- sprintf("reg_%s_%s.png", now, suffix)
  # tabOR %>% gt() %>% gtsave(filename=filename3, path="analysis/", vwidth=1200, vheight=800)
  tabOR %>%
    gtsummary::as_gt() %>%
    # gt::tab_header(title=paste(question)) %>%
    gt::tab_header(title=headr) %>%
    gt::gtsave(filename=filename3, path="analysis/", vwidth=1200, vheight=800)


  tabOR %>%
    gtsummary::as_gt() %>%
    # gt::tab_header(title=paste(question)) %>%
    gt::tab_header(title=headr) %>%
    gt::gtsave(filename=filename4, path="analysis/", vwidth=1200, vheight=800)