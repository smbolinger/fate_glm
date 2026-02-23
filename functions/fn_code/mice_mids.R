# mice:::plot.mids
if(FALSE){
  x=imp40
  y=NULL
  theme=mice.theme()
  layout = c(2,3)
  type="l"
  col=1:10
  lty=1
}
function (x, y = NULL, theme = mice.theme(), layout = c(2, 3), 
          type = "l", col = 1:10, lty = 1, ...) 
{
  strip.combined <- function(which.given, which.panel, factor.levels, 
                             ...) {
    if (which.given == 1) {
      lattice::panel.rect(0, 0, 1, 1, col = theme$strip.background$col, 
                          border = 1)
      lattice::panel.text(x = 0, y = 0.5, pos = 4, lab = factor.levels[which.panel[which.given]])
    }
    if (which.given == 2) {
      lattice::panel.text(x = 1, y = 0.5, pos = 2, lab = factor.levels[which.panel[which.given]])
    }
  }
  call <- match.call()
  if (!is.mids(x)) {
    stop("argument 'x' must be a 'mids' object", call. = FALSE)
  }
  if (is.null(x$chainMean)) {
    stop("no convergence diagnostics found", call. = FALSE)
  }
  mn <- x$chainMean
  sm <- sqrt(x$chainVar)
  obs <- apply(!(is.nan(mn) | is.na(mn)), 1, all)
  varlist <- names(obs)[obs]
  if (missing(y)) {
    formula <- as.formula(paste0(paste0(varlist, collapse = "+"), 
                                 "~.it|.ms"))
  }else {
    formula <- NULL
    if (is.null(y)) {
      formula <- as.formula(paste0(paste0(varlist, collapse = "+"), 
                                   "~.it|.ms"))
    }
    if (is.character(y)) {
      formula <- if (length(y) == 1) {
        as.formula(paste0(y, "~.it|.ms"))
      }
      else {
        as.formula(paste0(paste0(y, collapse = "+"), 
                          "~.it|.ms"))
      }
    }
    if (is.integer(y) || is.logical(y)) {
      vars <- varlist[y]
      formula <- if (length(vars) == 1) {
        as.formula(paste0(vars, "~.it|.ms"))
      }
      else {
        as.formula(paste0(paste0(vars, collapse = "+"), 
                          "~.it|.ms"))
      }
    }
    if (is.null(formula)) {
      formula <- as.formula(y)
    }
  }
  m <- x$m
  it <- x$iteration
  mn <- matrix(aperm(mn[varlist, , , drop = FALSE], c(2, 3, 
                                                      1)), nrow = m * it)
  sm <- matrix(aperm(sm[varlist, , , drop = FALSE], c(2, 3, 
                                                      1)), nrow = m * it)
  adm <- expand.grid(seq_len(it), seq_len(m), c("mean", "sd"))
  data <- cbind(adm, rbind(mn, sm))
  colnames(data) <- c(".it", ".m", ".ms", varlist)
  .m <- NULL
  rm(.m)
  tp <- lattice::xyplot(x = formula, data = data, groups = .m, 
                        type = type, lty = lty, col = col, layout = layout, scales = list(y = list(relation = "free"), 
                                                                                          x = list(alternating = FALSE)), as.table = TRUE, 
                        xlab = "Iteration", ylab = "", strip = strip.combined, 
                        par.strip.text = list(lines = 0.5))
                        # , ...)
  update(tp, par.settings = theme)
}
# <bytecode: 0x000001b12850ef30>
#   <environment: namespace:mice>

# > mice::mice.mids
if(FALSE){
  obj <- imp
  newdata=NULL
  maxit=35
  printFlag=TRUE
}
function (obj, newdata = NULL, maxit = 1, printFlag = TRUE, ...) 
{
  if (!is.mids(obj)) {
    stop("Object should be of type mids.")
  }
  assign(".Random.seed", obj$lastSeedValue, pos = 1)
  if (!is.null(newdata)) {
    ignore <- rep(FALSE, nrow(obj$data))
    if (!is.null(obj$ignore)) 
      ignore <- obj$ignore
    newdata <- check.newdata(newdata, obj$data)
    imp.newdata <- mice(newdata, m = obj$m, maxit = 0, remove.collinear = FALSE, 
                        remove.constant = FALSE)
    obj <- withCallingHandlers(rbind.mids(obj, imp.newdata), 
                               warning = function(w) {
                                 if (grepl("iterations differ", w$message)) {
                                   invokeRestart("muffleWarning")
                                 }
                               })
    obj$ignore <- c(ignore, rep(TRUE, nrow(newdata)))
  }
  if (maxit < 1) {
    return(obj)
  }
  loggedEvents <- obj$loggedEvents
  state <- list(it = 0, im = 0, co = 0, dep = "", meth = "", 
                log = !is.null(loggedEvents))
  if (is.null(loggedEvents)) {
    loggedEvents <- data.frame(it = 0, im = 0, co = 0, dep = "", 
                               meth = "", out = "")
  }
  call <- match.call()
  imp <- obj$imp
  where <- obj$where
  if (is.null(where)) 
    where <- is.na(obj$data)
  blocks <- obj$blocks
  if (is.null(blocks)) 
    blocks <- make.blocks(obj$data)
  sumIt <- obj$iteration + maxit
  from <- obj$iteration + 1
  to <- from + maxit - 1
  q <- mice:::sampler(obj$data, obj$m, obj$ignore, where, imp, blocks, 
               obj$method, obj$visitSequence, obj$predictorMatrix, obj$formulas, 
               obj$calltype, obj$blots, obj$post, c(from, to), printFlag)
               # ,  ...)
  imp <- q$imp
  vnames <- unique(unlist(obj$blocks))
  nvis <- length(vnames)
  if (!is.null(obj$chainMean)) {
    chainMean <- chainVar <- array(0, dim = c(nvis, to, obj$m), 
                                   dimnames = list(vnames, seq_len(to), paste("Chain", 
                                                                              seq_len(obj$m))))
    for (j in seq_len(nvis)) {
      if (obj$iteration == 0) {
        chainMean[j, , ] <- q$chainMean[j, , ]
        chainVar[j, , ] <- q$chainVar[j, , ]
      }
      else {
        chainMean[j, seq_len(obj$iteration), ] <- obj$chainMean[j, 
                                                                , ]
        chainVar[j, seq_len(obj$iteration), ] <- obj$chainVar[j, 
                                                              , ]
        chainMean[j, from:to, ] <- q$chainMean[j, , ]
        chainVar[j, from:to, ] <- q$chainVar[j, , ]
      }
    }
  } else {
    chainMean <- chainVar <- NULL
  }
  if (!state$log) {
    loggedEvents <- NULL
  }
  if (state$log) {
    row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  }
  midsobj <- mids(data = obj$data, imp = imp, m = obj$m, where = where, 
                  blocks = obj$blocks, call = call, nmis = obj$nmis, method = obj$method, 
                  predictorMatrix = obj$predictorMatrix, visitSequence = obj$visitSequence, 
                  formulas = obj$formulas, calltype = obj$calltype, post = obj$post, 
                  blots = obj$blots, ignore = obj$ignore, seed = obj$seed, 
                  iteration = sumIt, lastSeedValue = get(".Random.seed", 
                                                         envir = globalenv(), mode = "integer", inherits = FALSE), 
                  chainMean = chainMean, chainVar = chainVar, loggedEvents = loggedEvents)
  if (!is.null(newdata)) {
    include <- c(rep(FALSE, nrow(midsobj$data) - nrow(newdata)), 
                 rep(TRUE, nrow(newdata)))
    midsobj <- filter(midsobj, include)
  }
  return(midsobj)
}
# <bytecode: 0x000001b1530e6638>
  # <environment: namespace:mice>