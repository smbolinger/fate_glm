# psfmi::psfmi_lr
function (data, formula = NULL, nimp = 5, impvar = NULL, Outcome = NULL, 
          predictors = NULL, cat.predictors = NULL, spline.predictors = NULL, 
          int.predictors = NULL, keep.predictors = NULL, nknots = NULL, 
          p.crit = 1, method = "RR", direction = NULL) 
{
  if(FALSE){
    
          data=imp_comp 
                                nimp=m 
                                impvar=".imp" 
                                Outcome=resp
                                # predictors=c("nest_age", "obs_int", "fdate"), 
                                # predictors=c("species","nest_age", "obs_int", "fdate", "cam_fate"),
                                predictors=c("species","nest_age", "obs_int", "fdate")
                                cat.predictors = c("cam_fate")
                                int.predictors = inter
                                spline.predictors=NULL
                                method="D1"
  }
  call <- match.call()
  if (is_empty(formula)) {
    if (is_empty(Outcome)) 
      stop("Outcome variable not defined")
    P <- predictors
    cat.P <- cat.predictors
    int.P <- gsub(":", "*", int.predictors)
    s.P <- spline.predictors
  }
  else {
    form <- terms(formula)
    form_vars <- attr(form, "term.labels")
    if (is_empty(form_vars)) 
      stop("\n", "No predictors defined, model is empty")
    Outcome <- as.character(attr(form, "variables")[[2]])
    int.P <- form_vars[grepl(paste(c("[*]", ":"), collapse = "|"), 
                             form_vars)]
    int.P_temp <- unique(unlist(str_split(int.P, paste(c("[*]", 
                                                         ":"), collapse = "|"))))
    form_vars <- form_vars[!grepl(paste(c("[*]", ":"), collapse = "|"), 
                                  form_vars)]
    form_vars <- unique(c(form_vars, int.P_temp))
    cat.P <- form_vars[grepl("factor", form_vars)]
    form_vars <- form_vars[!grepl("factor", form_vars)]
    s.P <- form_vars[grepl("rcs", form_vars)]
    nknots <- c(readr::parse_number(s.P))
    form_vars <- form_vars[!grepl("rcs", form_vars)]
    int.P <- gsub(":", "*", clean_P(int.P))
    cat.P <- clean_P(cat.P)
    s.P <- clean_P(s.P)
    P <- form_vars
  }
  keep.P <- gsub(":", "*", keep.predictors)
  keep.P <- sapply(as.list(keep.P), clean_P)
  P.check <- c(P, cat.P, s.P)
  if (p.crit != 1) {
    if (is_empty(direction)) 
      stop("Specify FW or BW for forward or backward predictor selection")
  }
  if (!(is.data.frame(data))) 
    stop("Data should be a data frame")
  data <- data.frame(as_tibble(data))
  data <- mutate_if(data, is.factor, ~as.numeric(as.character(.x)))
  if (!all(data[Outcome] == 1 | data[Outcome] == 0)) 
    stop("Outcome should be a 0 - 1 variable")
  if ((nvar <- ncol(data)) < 2) 
    stop("Data should contain at least two columns")
  if (is_empty(impvar)) 
    stop("Imputation variable is not defined")
  if (is_empty(method)) 
    method = "RR"
  if (all(!is_empty(cat.P) | !is_empty(s.P)) & method == "RR") 
    stop("Categorical or spline variables in model, \n         define selection method: D1, D2, D3, D4 or MPR")
  if (sort(unique(data[, impvar]))[1] == 0) 
    stop("Original dataset should not be included")
  if (is_empty(nimp)) 
    stop("Number of imputed datasets is not defined, use nimp!")
  if (nimp < 2) {
    stop("\n", "Number of imputed datasets must be > 1", 
         "\n\n")
  }
  if (p.crit > 1) 
    stop("\n", "P-value criterium > 1", "\n")
  if (any(nknots < 3)) 
    stop("\n", "Number of knots must be > 2", "\n")
  if (length(nknots) != length(s.P)) 
    stop("\n", "Number of knots not specified for every spline variable", 
         "\n")
  if (!is_empty(cat.P)) {
    if (any(cat.P %in% P)) {
      cat.P.double <- cat.P[cat.P %in% P]
      stop("\n", "Categorical variable(s) -", cat.P.double, 
           "- also defined as Predictor", "\n\n")
    }
  }
  if (!is_empty(s.P)) {
    if (any(s.P %in% P)) {
      s.P.double <- s.P[s.P %in% P]
      stop("\n", "Do not include Spline variable(s) -", 
           s.P.double, "- in predictors", "\n\n")
    }
  }
  if (any(duplicated(P))) {
    stop("\n", "Predictor(s) - ", c(P[duplicated(P)]), " - defined more than once", 
         "\n\n")
  }
  if (any(!P.check %in% names(data))) {
    P.mis <- P.check[!P.check %in% names(data)]
    stop("\n", "Predictor(s) - ", P.mis, "- not available in dataset", 
         "\n\n")
  }
  if (!is_empty(int.P)) {
    int.P.check <- lapply(int.P[grep("[*]", int.P)], function(x) {
      unlist(strsplit(x, split = "[*]"))
    })
    int.P.check <- unique(unlist(int.P.check))
    if (any(!int.P.check %in% P.check)) 
      stop("\n", "Not all interaction terms defined as\n        Predictor or Categorical Predictor", 
           "\n\n")
  }
  P <- c(P, cat.P, s.P, int.P)
  if (is_empty(P)) 
    stop("\n", "No predictors to select, model is empty", 
         "\n\n")
  if (!is_empty(keep.P)) {
    for (i in 1:length(keep.P)) {
      if (grepl("[*]", keep.P[i])) {
        keep.P.spl <- unlist(strsplit(keep.P[i], split = "[*]"))
        if (length(P[Reduce("&", lapply(keep.P.spl, grepl, 
                                        P))]) == 0) 
          stop("Interaction term in keep.predictors not defined\n            as int.predictors, incorrect")
        keep.P[i] <- P[Reduce("&", lapply(keep.P.spl, 
                                          grepl, P))]
      }
    }
  }
  if (!is_empty(cat.P)) {
    if (length(cat.P) == 1) {
      P <- gsub(cat.P, replacement = paste0("factor(", 
                                            cat.P, ")"), P)
      if (!is_empty(keep.P)) {
        keep.P <- gsub(cat.P, replacement = paste0("factor(", 
                                                   cat.P, ")"), keep.P)
      }
    }
    else {
      for (i in 1:length(cat.P)) {
        P <- gsub(cat.P[i], replacement = paste0("factor(", 
                                                 cat.P[i], ")"), P)
        if (!is_empty(keep.P)) {
          keep.P <- gsub(cat.P[i], replacement = paste0("factor(", 
                                                        cat.P[i], ")"), keep.P)
        }
      }
    }
  }
  if (!is_empty(s.P)) {
    if (length(s.P) == 1) {
      P <- gsub(s.P, replacement = paste0("rcs(", s.P, 
                                          ",", nknots, ")"), P)
      if (!is_empty(keep.P)) {
        keep.P <- gsub(s.P, replacement = paste0("rcs(", 
                                                 s.P, ",", nknots, ")"), keep.P)
      }
    }
    else {
      for (i in 1:length(s.P)) {
        P <- gsub(s.P[i], replacement = paste0("rcs(", 
                                               s.P[i], ",", nknots[i], ")"), P)
        if (!is_empty(keep.P)) {
          keep.P <- gsub(s.P[i], replacement = paste0("rcs(", 
                                                      s.P[i], ",", nknots[i], ")"), keep.P)
        }
      }
    }
  }
  levels.cat.P <- lapply(cat.P, function(x) {
    nr.levels.cat.P <- length(table(data[data[impvar] == 
                                           1, ][, x]))
    if (nr.levels.cat.P < 3) {
      stop("\n", "Categorical variable(s) only 2 levels,\n        do not define as categorical", 
           "\n\n")
    }
  })
  if (any(!keep.P %in% P)) 
    stop("\n", "Variables to keep not defined as Predictor", 
         "\n\n")
  if (p.crit == 1) {
    pobjpool <- psfmi_lr_bw(data = data, nimp = nimp, impvar = impvar, 
                            Outcome = Outcome, P = P, p.crit = p.crit, method = method, 
                            keep.P = keep.P)
    class(pobjpool) <- "pmods"
    return(pobjpool)
  }
  if (direction == "FW") {
    pobjfw <- psfmi_lr_fw(data = data, nimp = nimp, impvar = impvar, 
                          Outcome = Outcome, p.crit = p.crit, P = P, keep.P = keep.P, 
                          method = method)
    class(pobjfw) <- "pmods"
    return(pobjfw)
  }
  if (direction == "BW") {
    pobjbw <- psfmi_lr_bw(data = data, nimp = nimp, impvar = impvar, 
                          Outcome = Outcome, P = P, p.crit = p.crit, method = method, 
                          keep.P = keep.P)
    class(pobjbw) <- "pmods"
    return(pobjbw)
  }
}
# <bytecode: 0x000001b1386fb038>
#   <environment: namespace:psfmi>
#   > 