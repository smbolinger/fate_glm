
# > mice::mice
mice_new <- function (data, m = 5, method = NULL, predictorMatrix, ignore = NULL, 
          where = NULL, blocks, visitSequence = NULL, formulas, calltype = NULL, 
          blots = NULL, post = NULL, defaultMethod = c("pmm", 
                                                       "logreg", "polyreg", "polr"), maxit = 5, 
          printFlag = TRUE, seed = NA, data.init = NULL, ...) 
{
  call <- match.call()
  check.deprecated(...)
  if (!is.na(seed)) 
    set.seed(seed)
  data <- mice:::check.dataform(data)
  m <- mice:::check.m(m)
  mp <- missing(predictorMatrix)
  mb <- missing(blocks)
  mf <- missing(formulas)
  if (mp & mb & mf) {
    blocks <- make.blocks(colnames(data))
    predictorMatrix <- make.predictorMatrix(data, blocks)
    formulas <- make.formulas(data, blocks)
    calltype <- make.calltype(calltype, predictorMatrix, 
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
    blocks <- mice:::check.blocks(blocks, data)
    formulas <- mice:::check.formulas(formulas, data)
    predictorMatrix <- mice:::check.predictorMatrix(predictorMatrix, 
                                             data, blocks)
    calltype <- mice:::make.calltype(calltype, predictorMatrix, 
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
  state <- list(it = 0, im = 0, dep = "", meth = "", 
                log = FALSE)
  loggedEvents <- data.frame(it = 0, im = 0, dep = "", 
                             meth = "", out = "")
  setup <- list(method = method, predictorMatrix = predictorMatrix, 
                visitSequence = visitSequence, post = post)
  setup <- mice:::mice.edit.setup(data, setup, user.visitSequence, 
                           ...)
  method <- setup$method
  predictorMatrix <- setup$predictorMatrix
  visitSequence <- setup$visitSequence
  post <- setup$post
  nmis <- apply(is.na(data), 2, sum)
  imp <- mice:::initialize.imp(data, m, ignore, where, blocks, visitSequence, 
                        method, nmis, data.init)
  from <- 1
  to <- from + maxit - 1
  q <- sampler_new(data, m, ignore, where, imp, blocks, method, 
               visitSequence, predictorMatrix, formulas, calltype, blots, 
               post, c(from, to), printFlag, ...)
  if (!state$log) 
    loggedEvents <- NULL
  if (state$log) 
    row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  midsobj <- mids(data = data, imp = q$imp, m = m, where = where, 
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
# <bytecode: 0x00000204915224b0>
#   <environment: namespace:mice>
#   
#   


# > mice:::sampler
sampler_new <- function (data, m, ignore, where, imp, blocks, method, visitSequence, 
          predictorMatrix, formulas, calltype, blots, post, fromto, 
          printFlag, ...) 
{
  # browser()
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
          univ <- !empt && !mice:::is.passive(theMethod) && 
            !mice:::handles.format(paste0("mice.impute.", 
                                   theMethod))
          mult <- !empt && !mice:::is.passive(theMethod) && 
            mice:::handles.format(paste0("mice.impute.", 
                                  theMethod))
          pass <- !empt && mice:::is.passive(theMethod) && length(blocks[[h]]) == 
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
              imp[[j]][, i] <- mice:::sampler.univ(data = data, 
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
            fm <- paste("mice.impute", theMethod, 
                        sep = ".")
            if (ct == "formula") {
              imputes <- do.call(fm, args = list(data = data, 
                                                 formula = ff, ...))
            }
            else if (ct == "pred") {
              imputes <- do.call(fm, args = list(data = data, 
                                                 type = pred, ...))
            }
            else {
              stop("Cannot call function of type ", 
                   ct, call. = FALSE)
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
      ridge.used <- any(grepl("A ridge penalty", 
                              r$out))
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
}
# <bytecode: 0x000002048b3d5a80>
#   <environment: namespace:mice>

# > mice:::
check.deprecated <- function (...) 
{
  nms <- names(list(...))
  replace.args <- list(imputationMethod = "method", defaultImputationMethod = "defaultMethod", 
                       form = "formulas")
  wrn <- names(replace.args) %in% nms
  if (any(wrn)) {
    for (i in which(wrn)) {
      msg <- paste0("The '", names(replace.args)[i], 
                    "' argument is no longer supported. Please use '", 
                    replace.args[i], "' instead.")
      warning(msg)
    }
  }
  invisible(NULL)
}
# <bytecode: 0x000002048b96da88>
#   <environment: namespace:mice>

if(FALSE){
      # imp <- mice::mice(ampDat, method=metList, m=m, print=FALSE)
  res <- mice_new(ampDat,method=metList,m=m,print=FALSE)
}