
# > mice:::
check.dataform <- function (data) 
{
  if (!(is.matrix(data) || is.data.frame(data))) {
    stop("Data should be a matrix or data frame", call. = FALSE)
  }
  if (ncol(data) < 2) {
    stop("Data should contain at least two columns", call. = FALSE)
  }
  data <- as.data.frame(data)
  mat <- sapply(data, is.matrix)
  df <- sapply(data, is.data.frame)
  if (any(mat)) {
    stop("Cannot handle columns with class matrix: ", colnames(data)[mat])
  }
  if (any(df)) {
    stop("Cannot handle columns with class data.frame: ", 
         colnames(data)[df])
  }
  dup <- duplicated(colnames(data))
  if (any(dup)) {
    stop("Duplicate names found: ", paste(colnames(data)[dup], 
                                          collapse = ", "))
  }
  data
}
# <bytecode: 0x000001b126797ca8>
#   <environment: namespace:mice>


#   q <- mice:::sampler(data, m, ignore, where, imp, blocks, method, 
#                visitSequence, predictorMatrix, formulas, calltype, blots, 
#                post, c(from, to), printFlag)
# # > mice:::sampler
function (data, m, ignore, where, imp, blocks, method, visitSequence, 
          predictorMatrix, formulas, calltype, blots, post, fromto, 
          printFlag, ...) 
{
  if(FALSE){
    fromto <- c(from,to)
    k=1
    h="species"
    j="species"
  }
  from <- fromto[1]
  to <- fromto[2]
  
  maxit <- to - from + 1
  r <- !is.na(data)
  chainMean <- chainVar <- mice:::initialize.chain(names(data), maxit, 
                                            m)
  if (maxit < 1) 
    iteration <- 0
  if (maxit >= 1) {
    if (printFlag) {
      cat("\n iter imp variable")
    }
    for (k in from:to) {
      iteration <- k
      for (i in seq_len(m)) {
        if (printFlag) {
          cat("\n ", iteration, " ", i)
        }
        for (h in visitSequence) {
          for (j in blocks[[h]]) {
            y <- data[, j]
            ry <- r[, j]
            wy <- where[, j]
            data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], 
                                            i]
          }
        }
        for (h in visitSequence) {
          ct <- calltype[[h]]
          b <- blocks[[h]]
          if (ct == "formula") 
            ff <- formulas[[h]]
          else ff <- NULL
          pred <- predictorMatrix[h, ]
          user <- blots[[h]]
          theMethod <- method[h]
          empt <- theMethod == ""
          univ <- !empt && !is.passive(theMethod) && 
            !handles.format(paste0("mice.impute.", theMethod))
          mult <- !empt && !is.passive(theMethod) && 
            handles.format(paste0("mice.impute.", theMethod))
          pass <- !empt && is.passive(theMethod) && length(blocks[[h]]) == 
            1
          if (printFlag & !empt) 
            cat(" ", b)
          oldstate <- get("state", pos = parent.frame())
          newstate <- list(it = k, im = i, dep = h, meth = theMethod, 
                           log = oldstate$log)
          assign("state", newstate, pos = parent.frame(), 
                 inherits = TRUE)
          if (univ) {
            for (j in b) {
              imp[[j]][, i] <- sampler.univ(data = data, 
                                            r = r, where = where, pred = pred, formula = ff, 
                                            method = theMethod, yname = j, k = k, 
                                            ct = ct, user = user, ignore = ignore, 
                                            ...)
              data[(!r[, j]) & where[, j], j] <- imp[[j]][(!r[, 
                                                              j])[where[, j]], i]
              cmd <- post[j]
              if (cmd != "") {
                eval(parse(text = cmd))
                data[(!r[, j]) & where[, j], j] <- imp[[j]][(!r[, 
                                                                j])[where[, j]], i]
              }
            }
          }
          if (mult) {
            mis <- !r
            mis[, setdiff(colnames(data), b)] <- FALSE
            data[mis] <- NA
            fm <- paste("mice.impute", theMethod, sep = ".")
            if (ct == "formula") {
              imputes <- do.call(fm, args = list(data = data, 
                                                 formula = ff, ...))
            }
            else if (ct == "pred") {
              imputes <- do.call(fm, args = list(data = data, 
                                                 type = pred, ...))
            }
            else {
              stop("Cannot call function of type ", ct, 
                   call. = FALSE)
            }
            if (is.null(imputes)) {
              stop("No imputations from ", theMethod, 
                   h, call. = FALSE)
            }
            for (j in names(imputes)) {
              imp[[j]][, i] <- imputes[[j]]
              data[!r[, j], j] <- imp[[j]][, i]
            }
          }
          if (pass) {
            for (j in b) {
              wy <- where[, j]
              ry <- r[, j]
              imp[[j]][, i] <- model.frame(as.formula(theMethod), 
                                           data[wy, ], na.action = na.pass)
              data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], 
                                              i]
            }
          }
        }
      }
      k2 <- k - from + 1L
      if (length(visitSequence) > 0L) {
        for (h in visitSequence) {
          for (j in blocks[[h]]) {
            if (!is.factor(data[, j])) {
              chainVar[j, k2, ] <- apply(imp[[j]], 2L, 
                                         var, na.rm = TRUE)
              chainMean[j, k2, ] <- colMeans(as.matrix(imp[[j]]), 
                                             na.rm = TRUE)
            }
            if (is.factor(data[, j])) {
              for (mm in seq_len(m)) {
                nc <- as.integer(factor(imp[[j]][, mm], 
                                        levels = levels(data[, j])))
                chainVar[j, k2, mm] <- var(nc, na.rm = TRUE)
                chainMean[j, k2, mm] <- mean(nc, na.rm = TRUE)
              }
            }
          }
        }
      }
    }
    if (printFlag) {
      r <- get("loggedEvents", parent.frame(1))
      ridge.used <- any(grepl("A ridge penalty", r$out))
      if (ridge.used) {
        cat("\n * Please inspect the loggedEvents \n")
      }
      else {
        cat("\n")
      }
    }
  }
  list(iteration = maxit, imp = imp, chainMean = chainMean, 
       chainVar = chainVar)
  if(FALSE){
    
    q <- list(iteration = maxit, imp = imp, chainMean = chainMean, 
       chainVar = chainVar)
  }
}
# <bytecode: 0x000001b123d4cd78>
#   <environment: namespace:mice>
#   

 # > mice::mice
function (data, m = 5, method = NULL, predictorMatrix, ignore = NULL, 
          where = NULL, blocks, visitSequence = NULL, formulas, calltype = NULL, 
          blots = NULL, post = NULL, defaultMethod = c("pmm", "logreg", 
                                                       "polyreg", "polr"), maxit = 5, printFlag = TRUE, seed = NA, 
          data.init = NULL, ...) 
{
  call <- match.call()
  check.deprecated(...)
  if (!is.na(seed)) 
    set.seed(seed)
  if(debugging){
    data <- ampDat
    m <- 20
    where <- NULL
    calltype <- NULL
    method <- NULL
    visitSequence <- NULL
    defaultMethod = c("pmm", "logreg", 
                      "polyreg", "polr")
    maxit <- 5
    post <- NULL
    ignore <- NULL
    blots <- NULL
    printFlag <- TRUE
    seed <- NA
    data.init <- NULL
  }
  data <- mice:::check.dataform(data)
  m <- mice:::check.m(m)
  mp <- missing(predictorMatrix)
  mb <- missing(blocks)
  mf <- missing(formulas)
  if (mp & mb & mf) {
    # can't get mp, mb etc to work within the function so just run these if they're all missing:
    blocks <- mice::make.blocks(colnames(data))
    predictorMatrix <- mice::make.predictorMatrix(data, blocks)
    formulas <- mice::make.formulas(data, blocks)
    calltype <- mice::make.calltype(calltype, predictorMatrix, 
                              formulas, "pred")
  }
  if (!mp & mb & mf) {
    predictorMatrix <- check.predictorMatrix(predictorMatrix, 
                                             data)
    blocks <- make.blocks(colnames(predictorMatrix), partition = "scatter")
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
    calltype <- make.calltype(calltype, predictorMatrix, 
                              formulas, "pred")
  }
  if (mp & !mb & mf) {
    blocks <- check.blocks(blocks, data)
    predictorMatrix <- make.predictorMatrix(data, blocks)
    formulas <- make.formulas(data, blocks)
    calltype <- make.calltype(calltype, predictorMatrix, 
                              formulas, "pred")
  }
  if (mp & mb & !mf) {
    formulas <- check.formulas(formulas, data)
    blocks <- construct.blocks(formulas)
    predictorMatrix <- make.predictorMatrix(data, blocks)
    calltype <- make.calltype(calltype, predictorMatrix, 
                              formulas, "formula")
  }
  if (!mp & !mb & mf) {
    blocks <- check.blocks(blocks, data)
    z <- check.predictorMatrix(predictorMatrix, data, blocks)
    predictorMatrix <- z$predictorMatrix
    blocks <- z$blocks
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
    calltype <- make.calltype(calltype, predictorMatrix, 
                              formulas, "pred")
  }
  if (!mp & mb & !mf) {
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, 
                                             data)
    blocks <- construct.blocks(formulas, predictorMatrix)
    predictorMatrix <- make.predictorMatrix(data, blocks, 
                                            predictorMatrix)
    calltype <- make.calltype(calltype, predictorMatrix, 
                              formulas, "formula")
  }
  if (mp & !mb & !mf) {
    blocks <- check.blocks(blocks, data)
    formulas <- check.formulas(formulas, blocks)
    predictorMatrix <- make.predictorMatrix(data, blocks)
    calltype <- make.calltype(calltype, predictorMatrix, 
                              formulas, "formula")
  }
  if (!mp & !mb & !mf) {
    blocks <- check.blocks(blocks, data)
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, 
                                             data, blocks)
    calltype <- make.calltype(calltype, predictorMatrix, 
                              formulas, "formula")
  }
  chk <- mice:::check.cluster(data, predictorMatrix)
  where <- mice:::check.where(where, data, blocks)
  user.visitSequence <- visitSequence
  visitSequence <- mice:::check.visitSequence(visitSequence, data = data, 
                                       where = where, blocks = blocks)
  predictorMatrix <- mice:::mice.edit.predictorMatrix(predictorMatrix = predictorMatrix, 
                                               visitSequence = visitSequence, user.visitSequence = user.visitSequence, 
                                               maxit = maxit)
  method <- mice:::check.method(method = method, data = data, where = where, 
                         blocks = blocks, defaultMethod = defaultMethod)
  post <- mice:::check.post(post, data)
  blots <- mice:::check.blots(blots, data, blocks)
  ignore <- mice:::check.ignore(ignore, data)
  state <- list(it = 0, im = 0, dep = "", meth = "", log = FALSE)
  loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", 
                             out = "")
  setup <- list(method = method, predictorMatrix = predictorMatrix, 
                visitSequence = visitSequence, post = post)
  if(FALSE){
    setup <- mice:::mice.edit.setup(data, setup, user.visitSequence)
  }
  setup <- mice:::mice.edit.setup(data, setup, user.visitSequence ,  ...)
  method <- setup$method
  predictorMatrix <- setup$predictorMatrix
  visitSequence <- setup$visitSequence
  post <- setup$post
  nmis <- apply(is.na(data), 2, sum)
  imp <- mice:::initialize.imp(data, m, ignore, where, blocks, visitSequence, 
                        method, nmis, data.init)
  from <- 1
  to <- from + maxit - 1
  q <- mice:::sampler(data, m, ignore, where, imp, blocks, method, 
               visitSequence, predictorMatrix, formulas, calltype, blots, 
               post, c(from, to), printFlag , ...)
  if (!state$log) 
    loggedEvents <- NULL
  if (state$log) 
    row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  midsobj <- mice::mids(data = data, imp = q$imp, m = m, where = where, 
                  blocks = blocks, call = call, nmis = nmis, method = method, 
                  predictorMatrix = predictorMatrix, visitSequence = visitSequence, 
                  formulas = formulas, calltype = calltype, post = post, 
                  blots = blots, ignore = ignore, seed = seed, iteration = q$iteration, 
                  lastSeedValue = get(".Random.seed", envir = globalenv(), 
                                      mode = "integer", inherits = FALSE), chainMean = q$chainMean, 
                  chainVar = q$chainVar, loggedEvents = loggedEvents)
  if (!is.null(midsobj$loggedEvents)) {
    warning("Number of logged events: ", nrow(midsobj$loggedEvents), 
            call. = FALSE)
  }
  return(midsobj)
}
# <bytecode: 0x000001b118c1edd8>
#   <environment: namespace:mice>
#   

# > mice:::augment
function (y, ry, x, wy, maxcat = 50) 
{
  icod <- sort(unique(unclass(y)))
  k <- length(icod)
  if (k > maxcat) {
    stop("Maximum number of categories (", maxcat, ") exceeded")
  }
  p <- ncol(x)
  if (p == 0) {
    return(list(y = y, ry = ry, x = x, wy = wy, w = rep(1, 
                                                        length(y))))
  }
  if (sum(!ry) == 1) {
    return(list(y = y, ry = ry, x = x, wy = wy, w = rep(1, 
                                                        length(y))))
  }
  mean <- apply(x, 2, mean, na.rm = TRUE)
  sd <- sqrt(apply(x, 2, var, na.rm = TRUE))
  minx <- apply(x, 2, min, na.rm = TRUE)
  maxx <- apply(x, 2, max, na.rm = TRUE)
  nr <- 2 * p * k
  a <- matrix(mean, nrow = nr, ncol = p, byrow = TRUE)
  b <- matrix(rep(c(rep.int(c(0.5, -0.5), k), rep.int(0, nr)), 
                  length = nr * p), nrow = nr, ncol = p, byrow = FALSE)
  c <- matrix(sd, nrow = nr, ncol = p, byrow = TRUE)
  d <- a + b * c
  d <- pmax(matrix(minx, nrow = nr, ncol = p, byrow = TRUE), 
            d, na.rm = TRUE)
  d <- pmin(matrix(maxx, nrow = nr, ncol = p, byrow = TRUE), 
            d, na.rm = TRUE)
  e <- rep(rep(icod, each = 2), p)
  dimnames(d) <- list(paste0("AUG", seq_len(nrow(d))), dimnames(x)[[2]])
  xa <- rbind.data.frame(x, d)
  ya <- if (is.factor(y)) {
    if (is.ordered(y)) {
      ordered(levels(y)[c(y, e)], levels = levels(y))
    }
    else {
      as.factor(levels(y)[c(y, e)])
    }
  }
  else c(y, e)
  rya <- c(ry, rep.int(TRUE, nr))
  wya <- c(wy, rep.int(FALSE, nr))
  wa <- c(rep.int(1, length(y)), rep.int((p + 1)/nr, nr))
  list(y = ya, ry = rya, x = xa, w = wa, wy = wya)
}
# <bytecode: 0x000001610a40b358>
#   <environment: namespace:mice>

# > mice::mice.impute.polyreg
# function (y, ry, x,
y = ampDat$cam_fate # vector to be imputed
ry = !is.na(ampDat$cam_fate) # FALSE if missing
# colnames(ampDat)
ampDat$speciesCONI = ifelse(ampDat$species=="CONI", 1, 0)
x = ampDat[,c(3:7)] # design matrix
wy = NULL
nnet.maxit = 100
nnet.trace = FALSE 
nnet.MaxNWts = 1500
# ...) 
{
  if (is.null(wy)) {
    wy <- !ry
  }
  x <- as.matrix(x)
  aug <- mice:::augment(y, ry, x, wy)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  wy <- aug$wy
  w <- aug$w
  fy <- as.factor(y)
  nc <- length(levels(fy))
  un <- rep(runif(sum(wy)), each = nc)
  xy <- cbind.data.frame(y = y, x = x)
  if (ncol(x) == 0L) {
    xy <- data.frame(xy, int = 1)
  }
  cat.has.all.obs <- table(y[ry]) == sum(ry)
  if (any(cat.has.all.obs)) {
    return(rep(levels(fy)[cat.has.all.obs], sum(wy)))
  }
  fit <- nnet::multinom(formula(xy), data = xy[ry, , drop = FALSE], 
                        weights = w[ry], maxit = nnet.maxit, trace = nnet.trace, 
                        MaxNWts = nnet.MaxNWts, ...)
  post <- predict(fit, xy[wy, , drop = FALSE], type = "probs")
  if (sum(wy) == 1) {
    post <- matrix(post, nrow = 1, ncol = length(post))
  }
  if (is.vector(post)) {
    post <- matrix(c(1 - post, post), ncol = 2)
  }
  draws <- un > apply(post, 1, cumsum)
  idx <- 1 + apply(draws, 2, sum)
  levels(fy)[idx]
}
# <bytecode: 0x000001610adeb698>
#   <environment: namespace:mice>