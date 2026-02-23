# > mice:::cbind.mids.mids
function (x, y, call) 
{
  if (!is.mids(y)) 
    stop("Argument `y` not a mids object")
  if (nrow(y$data) != nrow(x$data)) {
    stop("The two datasets do not have the same length\n")
  }
  if (x$m != y$m) {
    stop("The two mids objects should have the same number of imputations\n")
  }
  call <- c(x$call, call)
  data <- cbind(x$data, y$data)
  xynames <- c(colnames(x$data), colnames(y$data))
  varnames <- make.unique(xynames)
  names(varnames) <- xynames
  names(data) <- varnames
  where <- cbind(x$where, y$where)
  colnames(where) <- varnames
  xnew <- varnames[1:ncol(x$data)]
  ynew <- varnames[-(1:ncol(x$data))]
  xblocks <- x$blocks
  yblocks <- y$blocks
  for (i in names(xblocks)) xblocks[[i]] <- unname(xnew[xblocks[[i]]])
  for (i in names(yblocks)) yblocks[[i]] <- unname(ynew[yblocks[[i]]])
  blocks <- c(xblocks, yblocks)
  xynames <- c(names(xblocks), names(yblocks))
  blocknames <- make.unique(xynames)
  names(blocknames) <- xynames
  names(blocks) <- blocknames
  calltype <- c(x$calltype, y$calltype)
  names(calltype) <- blocknames
  m <- x$m
  nmis <- c(x$nmis, y$nmis)
  names(nmis) <- varnames
  imp <- c(x$imp, y$imp)
  names(imp) <- varnames
  method <- c(x$method, y$method)
  names(method) <- blocknames
  if (all(names(ynew) == unname(ynew)) && all(names(xnew) == 
                                              unname(xnew))) {
    formulas <- c(x$formulas, y$formulas)
  }
  else {
    xformulas <- x$formulas
    yformulas <- y$formulas
    for (i in names(xformulas)) {
      xformulas[[i]] <- renf(xformulas[[i]], xnew)
    }
    for (i in names(yformulas)) {
      yformulas[[i]] <- renf(yformulas[[i]], ynew)
    }
    formulas <- c(xformulas, yformulas)
  }
  names(formulas) <- blocknames
  predictorMatrix <- rbind(x$predictorMatrix, matrix(0, ncol = ncol(x$predictorMatrix), 
                                                     nrow = nrow(y$predictorMatrix)))
  predictorMatrix <- cbind(predictorMatrix, rbind(matrix(0, 
                                                         ncol = ncol(y$predictorMatrix), nrow = nrow(x$predictorMatrix)), 
                                                  y$predictorMatrix))
  rownames(predictorMatrix) <- blocknames
  colnames(predictorMatrix) <- varnames
  xnew <- blocknames[1:length(x$blocks)]
  ynew <- blocknames[-(1:length(x$blocks))]
  visitSequence <- unname(c(xnew[x$visitSequence], ynew[y$visitSequence]))
  post <- c(x$post, y$post)
  names(post) <- varnames
  blots <- c(x$blots, y$blots)
  names(blots) <- blocknames
  ignore <- x$ignore
  seed <- x$seed
  lastSeedValue <- x$lastSeedValue
  iteration <- x$iteration
  chainMean <- array(data = NA, dim = c(dim(x$chainMean)[1] + 
                                          dim(y$chainMean)[1], iteration, m), dimnames = list(c(dimnames(x$chainMean)[[1]], 
                                                                                                dimnames(y$chainMean)[[1]]), dimnames(x$chainMean)[[2]], 
                                                                                              dimnames(x$chainMean)[[3]]))
  chainMean[seq_len(dim(x$chainMean)[1]), , ] <- x$chainMean
  if (iteration <= dim(y$chainMean)[2]) {
    chainMean[(dim(x$chainMean)[1] + 1):dim(chainMean)[1], 
              , ] <- y$chainMean[, seq_len(iteration), ]
  }
  else {
    chainMean[(dim(x$chainMean)[1] + 1):dim(chainMean)[1], 
              seq_len(dim(y$chainMean)[2]), ] <- y$chainMean
  }
  chainVar <- array(data = NA, dim = c(dim(x$chainVar)[1] + 
                                         dim(y$chainVar)[1], iteration, m), dimnames = list(c(dimnames(x$chainVar)[[1]], 
                                                                                              dimnames(y$chainVar)[[1]]), dimnames(x$chainVar)[[2]], 
                                                                                            dimnames(x$chainVar)[[3]]))
  chainVar[seq_len(dim(x$chainVar)[1]), , ] <- x$chainVar
  if (iteration <= dim(y$chainVar)[2]) {
    chainVar[(dim(x$chainVar)[1] + 1):dim(chainVar)[1], , 
    ] <- y$chainVar[, seq_len(iteration), ]
  }
  else {
    chainVar[(dim(x$chainVar)[1] + 1):dim(chainVar)[1], seq_len(dim(y$chainVar)[2]), 
    ] <- y$chainVar
  }
  loggedEvents <- x$loggedEvents
  midsobj <- mids(data = data, imp = imp, m = m, where = where, 
                  blocks = blocks, call = call, nmis = nmis, method = method, 
                  predictorMatrix = predictorMatrix, visitSequence = visitSequence, 
                  formulas = formulas, calltype = calltype, post = post, 
                  blots = blots, ignore = ignore, seed = seed, iteration = iteration, 
                  lastSeedValue = lastSeedValue, chainMean = chainMean, 
                  chainVar = chainVar, loggedEvents = loggedEvents)
  return(midsobj)
}
# <bytecode: 0x000001b14e9de330>
  # <environment: namespace:mice>