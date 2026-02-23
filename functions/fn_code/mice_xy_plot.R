# > mice::xyplot
# function (x, data, ...) 
#   UseMethod("xyplot")
# <bytecode: 0x00000291060602d8>
#   <environment: namespace:lattice>
#   > mice::xyplot.mids
# Error: 'xyplot.mids' is not an exported object from 'namespace:mice'
# 
# > mice:::xyplot.mids
xy_plot <- function (x, data, na.groups = NULL, groups = NULL, as.table = TRUE,
          theme = mice.theme(), allow.multiple = TRUE, outer = TRUE, 
          drop.unused.levels = lattice::lattice.getOption("drop.unused.levels"), 
          ..., subscripts = TRUE, subset = TRUE) 
{
  call <- match.call()
  if (!is.mids(x)) 
    stop("Argument 'x' must be a 'mids' object")
  if (missing(data)) 
    stop("Missing formula")
  formula <- data
  cd <- data.frame(complete(x, "long", include = TRUE))
  r <- as.data.frame(is.na(x$data))
  nagp <- eval(expr = substitute(na.groups), envir = r, enclos = parent.frame())
  if (is.expression(nagp)) {
    nagp <- eval(expr = nagp, envir = r, enclos = parent.frame())
    }
  ngp <- eval(expr = substitute(groups), envir = cd, enclos = parent.frame())
  if (is.expression(ngp)){ 
    ngp <- eval(expr = ngp, envir = cd, enclos = parent.frame())
    }
  groups <- ngp
  ss <- eval(expr = substitute(subset), envir = cd, enclos = parent.frame())
  if (is.expression(ss)) 
    ss <- eval(expr = ss, envir = cd, enclos = parent.frame())
  subset <- ss
  dots <- list(...)
  args <- list(allow.multiple = allow.multiple, outer = outer, 
               drop.unused.levels = drop.unused.levels, subscripts = subscripts, 
               as.table = as.table)
  form <- lattice::latticeParseFormula(model = formula, data = cd, 
                                       subset = subset, groups = groups, multiple = allow.multiple, 
                                       outer = outer, subscripts = TRUE, drop = drop.unused.levels)
  ynames <- unlist(lapply(strsplit(form$left.name, " \\+ "), 
                          rm.whitespace))
  nona <- is.null(call$na.groups)
  if (!is.null(call$groups) && nona) {
    gp <- call$groups
  } else {
    if (nona) {
      na.df <- r[, ynames, drop = FALSE]
      gp <- unlist(lapply(na.df, rep.int, x$m + 1))
    }
    else {
      gp <- rep.int(nagp, length(ynames) * (x$m + 1))
    }
  }
  if (is.null(call$ylab)) {
    args$ylab <- ""
    if (length(ynames) == 1) 
      args$ylab <- ynames
  }
  if (is.null(call$scales)) {
    args$scales <- list()
    if (length(ynames) > 1) {
      args$scales <- list(x = list(relation = "free"), 
                          y = list(relation = "free"))
    }
  }
  args <- c(x = formula, data = list(cd), groups = list(gp), 
            args, dots, subset = call$subset)
  tp <- do.call(lattice::xyplot, args)
  update(tp, par.settings = theme)
}
# <bytecode: 0x000002912aa548b0>
#   <environment: namespace:mice>