"screeplot.dudi" <- function(x, col.kept = "grey", col = "white", pos = -1, plot = TRUE, ...) {
  if(!inherits(x, "dudi")) 
    stop("Object of class 'dudi' expected")
  
  ## prepare
  nf <- 1:x$nf
  col <- rep(col, length(x$eig))
  col[nf] <- col.kept
  
  ## default values for parameters 
  sortparameters <- sortparamADEg(...)
  params <- list()
  params$adepar <- list(ppolygons = list(col = col), porigin = list(origin = c(0, 0)), pgrid = list(draw = FALSE), p1d = list(horizontal = FALSE), paxes = list(draw = TRUE, x = list(draw = FALSE)))
  params$g.args <- list(main = deparse(substitute(x)), xlab = "Axis", ylab = "Inertia", ylim = c(min(0, min(x$eig)), max(x$eig) * 1.1))
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## ADEg creation
  object <- do.call("s1d.barchart", c(list(score = substitute(x$eig), pos = pos - 2, plot = FALSE), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args))
  object@Call <- match.call()
  if(plot)
    print(object)
  invisible(object)
}


"biplot.dudi" <- function(x, pos = -1, plot = TRUE, ...) {
  if(!inherits(x, "dudi")) 
    stop("Object of class 'dudi' expected")
  
  object <- do.call("scatter", c(list(substitute(x), pos = pos - 3, plot = FALSE, ...)))
  object@Call <- match.call()
  if(plot)
    print(object)
  invisible(object)
}


"plot.acm"  <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "dudi"))
    stop("Object of class 'dudi' expected")
  if(!inherits(x, "acm"))
    stop("Object of class 'acm' expected")
  
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  ## prepare
  oritab <- as.list(x$call)[[2]]
  
  ## parameter management
  sortparameters <- sortparamADEg(...)
  params <- list()
  params$g.args <- list(starSize = 0)
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  object <- do.call("s.class", c(list(dfxy = substitute(x$li), fac = oritab, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
  object@Call <- match.call()
  if(plot)
    print(object)
  invisible(object)
}


"plot.fca" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "dudi"))
    stop("Object of class 'dudi' expected")
  if(!inherits(x, "fca"))
    stop("Object of class 'fca' expected")
  
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  ## prepare
  oritab <- as.list(x$call)[[2]]
  evTab <- eval.parent(oritab)
  indica <- factor(rep(names(x$blo), x$blo))
  ng <- length(levels(indica))   
  
  ## parameter management
  graphsnames <- as.character(levels(indica))
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  params <- list()
  params <- lapply(1:length(graphsnames), function(i) {params[[i]] <- list(starSize = 0.5, ellipseSize = 0, plabels = list(cex = 1.25), psub = list(text = graphsnames[i]))})
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  l <- list()
  l <- sapply(1:length(levels(indica)), function(i) {do.call("s.distri", c(list(dfxy = substitute(x$l1, env = sys.frame(-3)), dfdistri = call("[", oritab, call(":", 1, nrow(evTab)), which(indica == levels(indica)[i])), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[i]]))})
  
  ## ADEgS creation
  object <- new(Class = "ADEgS", ADEglist = l, positions = layout2position(.n2mfrow(ng), ng  = ng), add = matrix(0, ncol = ng, nrow = ng), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.coinertia" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "coinertia")) 
    stop("Object of class 'coinertia' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("Xax", "Yax", "eig", "XYmatch", "Yloadings", "Xloadings")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Unconstrained axes (X)"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Unconstrained axes (Y)"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list(psub = list(text = "Row scores (X -> Y)"))
  params[[5]] <- list(psub = list(text = "Y loadings"), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "X loadings"), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## Creation of each individual ADEg
  g1 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aX), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aY), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.match", c(list(dfxy1 = substitute(x$mX), dfxy2 = substitute(x$mY), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  g5 <- do.call("s.arrow", c(list(dfxy = substitute(x$l1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]])) 
  g6 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6), positions = layout2position(lay), add = matrix(0, ncol = 6, nrow = 6), Call = match.call() )
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.pcaiv" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "pcaiv")) 
    stop("Object of class 'pcaiv' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("Xloadings", "Xcor", "eig", "XYmatch", "Yax", "Ycol")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "X loadings"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "X correlation"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list(psub = list(text = "Predictions (X) -> Scores (Y)"))
  params[[5]] <- list(psub = list(text = "Unconstrained axes (Y)"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "Y columns"), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## Creation of each individual ADEg
  g1 <- do.call("s.arrow", c(list(dfxy = substitute(na.omit(x$fa)), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.corcircle", c(list(dfxy = substitute(na.omit(x$cor)), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.match", c(list(dfxy1 = substitute(x$li), dfxy2 = substitute(x$ls), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]] ))
  g5 <- do.call("s.corcircle", c(list(dfxy = substitute(x$as), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g6 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6), positions = layout2position(lay), add = matrix(0, ncol = 6, nrow = 6), Call = match.call() )
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.betcoi" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "betcoi"))
    stop("Object of class 'betcoi' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  fac <- eval.parent(appel$fac)
  
  ## sort parameters for each graph
  graphsnames <- c("Xax", "Yax", "eig", "XYmatch", "Yloadings", "Xloadings")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(1, 1, 1, 3, 1, 1))
  
  ## compute limits for the ADEgS 'XYmatch' (two s.class and one s.match)
  mat <- rbind(x$msX, x$msY, x$mX)
  minmat <- apply(mat, 2, min)
  maxmat <- apply(mat, 2, max)
  limdefault <- setlimits2D(minmat[1], maxmat[1], minmat[2], maxmat[2], origin = c(0, 0), includeOr = TRUE)     
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Unconstrained axes (X)"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Unconstrained axes (Y)"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list()
  params[[4]]$l1 <- list(psub = list(text = "Row scores (X -> Y)"), xlim = limdefault$xlim, ylim = limdefault$ylim, chullSize = 1, ppoints = list(pch = 16, cex = 0.5), plines = list(lwd = 1), plabels = list(alpha = 0, boxes = list(draw = FALSE)), ppolygon = list(lwd = 0.5, alpha = 0.2), pellipses = list(alpha = 0, axes = list(draw = FALSE)), col = adegpar()$ppalette$quali(nlevels(fac)))
  params[[4]]$l2 <- list(xlim = limdefault$xlim, ylim = limdefault$ylim, chullSize = 1, ppoints = list(pch = 15, cex = 0.5), plines = list(lwd = 1), plabels = list(alpha = 0, boxes = list(draw = FALSE)), ppolygon = list(lwd = 0.5, alpha = 0.2), pellipses = list(alpha = 0.0, axes = list(draw = FALSE)), col = adegpar()$ppalette$quali(nlevels(fac)))
  params[[4]]$l3 <- list(xlim = limdefault$xlim, ylim = limdefault$ylim, ppoints = list(cex = 0.7), plines = list(lwd = 2), plabels = list(alpha = 1, boxes = list(draw = TRUE), cex = 1.25))
  params[[5]] <- list(psub = list(text = "Y loadings"), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "X loadings"), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.arrow", c(list(dfxy = substitute(x$aX), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.arrow", c(list(dfxy = substitute(x$aY), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]])) 
  g41 <- do.call("s.class", c(list(dfxy = substitute(x$msX), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]][[1]]))
  g42 <- do.call("s.class", c(list(dfxy = substitute(x$msY), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]][[2]]))
  g43 <- do.call("s.match", c(list(dfxy1 = substitute(x$mX), dfxy2 = substitute(x$mY), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]][[3]]))
  g4 <- do.call("superpose", list(g41, g42))
  g4@Call <- call("superpose", g41@Call, g42@Call)
  g4 <- do.call("superpose", list(g4, g43))
  g4@Call <- call("superpose", g4@Call, g43@Call)
  g5 <- do.call("s.arrow", c(list(dfxy = substitute(x$l1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]])) 
  g6 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6), positions = layout2position(lay), add = matrix(0, ncol = 6, nrow = 6), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.betrlq" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "betrlq")) 
    stop("Object of class 'betrlq' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  
  ## sort parameters for each graph
  graphsnames <- c("Rrow", "Qrow", "Rax", "Rloadings", "Qloadings", "Qax", "eig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "R row scores and classes"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Q row scores"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Unconstrained axes (R)"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[4]] <- list(psub = list(text = "R loadings"), plabels = list(cex = 1.25))
  params[[5]] <- list(psub = list(text = "Q loadings"), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "Unconstrained axes (Q)"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[7]] <- list(psub = list(text = "Eigenvalues"))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.class", c(list(dfxy = substitute(x$lsR), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.label", c(list(dfxy = substitute(x$lQ), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aR), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.arrow", c(list(dfxy = substitute(x$l1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  g5 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  g6 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aQ), xax, yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g7 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[7]])) 
  
  ## ADEgS creation
  lay <- matrix(c(1, 1, 3, 1, 1, 4, 2, 2, 5, 2, 2, 6, 0, 0, 7), 3, 5)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6, g7), positions = layout2position(lay), add = matrix(0, ncol = 7, nrow = 7), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.between" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "between")) 
    stop("Object of class 'between' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  
  ## sort parameters for each graph
  graphsnames <- c("loadings", "col", "eig", "row", "Xax", "class")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Loadings"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Columns"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list(psub = list(text = "Row scores and classes"), plabels = list(cex = 1.25))
  params[[5]] <- list(psub = list(text = "Unconstrained axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "Classes"), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.arrow", c(list(dfxy = substitute(x$co), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]])) 
  g4 <- do.call("s.class", c(list(dfxy = substitute(x$ls), wt = call("$", appel$x, "lw"), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  g5 <- do.call("s.corcircle", c(list(dfxy = substitute(x$as), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g6 <- do.call("s.label", c(list(dfxy = substitute(x$li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6), positions = layout2position(lay), add = matrix(0, ncol = 6, nrow = 6), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.discrimin" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "discrimin")) 
    stop("Object of class 'discrimin' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  
  ## sort parameters for each graph  
  graphsnames <- c("loadings", "col", "eig", "row", "Xax", "class")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Loadings"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Columns"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list(psub = list(text = "Row scores and classes"), plabels = list(cex = 1.25))
  params[[5]] <- list(psub = list(text = "Unconstrained axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "Classes scores"), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.arrow", c(list(dfxy = substitute(x$fa), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.corcircle", c(list(dfxy = substitute(x$va), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]])) 
  g4 <- do.call("s.class", c(list(dfxy = substitute(x$li), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  g5 <- do.call("s.corcircle", c(list(dfxy = substitute(x$cp), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g6 <- do.call("s.label", c(list(dfxy = substitute(x$gc), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6), positions = layout2position(lay), add = matrix(0, ncol = 6, nrow = 6), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.within" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "within")) 
    stop("Object of class 'within' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  
  ## sort parameters for each graph
  graphsnames <- c("loadings", "col", "eig", "row", "Xax", "ccrow")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Loadings"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Columns"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list(psub = list(text = "Row scores and classes"), plabels = list(cex = 1.25))
  params[[5]] <- list(psub = list(text = "Unconstrained axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "Row scores (common centring)"), pellipses = list(axes = list(draw = FALSE)), plines = list(lwd = 0), plabels = list(alpha = 0, boxes = list(draw = FALSE), cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.arrow", c(list(dfxy = substitute(x$co), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.class", c(list(dfxy = substitute(x$ls), wt = substitute(x$lw), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  g5 <- do.call("s.corcircle", c(list(dfxy = substitute(x$as), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g6 <- do.call("s.class", c(list(dfxy = substitute(x$li), wt = substitute(x$lw), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6), positions = layout2position(lay), add = matrix(0, ncol = 6, nrow = 6), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.witcoi" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "witcoi"))
    stop("Object of class 'witcoi' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  fac <- eval.parent(appel$fac)
  
  ## sort parameters for each graph
  graphsnames <- c("Xax", "Yax", "eig", "XYmatch", "Yloadings", "Xloadings")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(1, 1, 1, 3, 1, 1))
  
  ## compute limits for the ADEgS (two s.class and one s.match)
  mat <- rbind(x$msX, x$msY, x$mX)
  minmat <- apply(mat, 2, min)
  maxmat <- apply(mat, 2, max)
  limdefault <- setlimits2D(minmat[1], maxmat[1], minmat[2], maxmat[2], origin = c(0, 0), includeOr = TRUE)     
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Unconstrained axes (X)"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Unconstrained axes (Y)"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list()
  params[[4]]$l1 <- list(psub = list(text = "Row scores (X -> Y)"), xlim = limdefault$xlim, ylim = limdefault$ylim, chullSize = 1, ppoints = list(pch = 16, cex = 0.5), plabels = list(alpha = 0, boxes = list(draw = FALSE)), ppolygon = list(lwd = 0.5, alpha = 0.2), pellipses = list(alpha = 0.0, axes = list(draw = FALSE)), col = adegpar()$ppalette$quali(nlevels(fac)))
  params[[4]]$l2 <- list(xlim = limdefault$xlim, ylim = limdefault$ylim, chullSize = 1, ppoints = list(pch = 15, cex = 0.5), plabels = list(alpha = 0, boxes = list(draw = FALSE)), ppolygon = list(lwd = 0.5, alpha = 0.2), pellipses = list(alpha = 0.0, axes = list(draw = FALSE)), col = adegpar()$ppalette$quali(nlevels(fac)))
  params[[4]]$l3 <- list(xlim = limdefault$xlim, ylim = limdefault$ylim, ppoints = list(cex = 0.7), plines = list(lwd = 2), plabels = list(alpha = 1, boxes = list(draw = TRUE), cex = 1.25))
  params[[5]] <- list(psub = list(text = "Y loadings"), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "X loadings"), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.arrow", c(list(dfxy = substitute(x$aX), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.arrow", c(list(dfxy = substitute(x$aY), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))  
  g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]])) 
  g41 <- do.call("s.class", c(list(dfxy = substitute(x$msX), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]][[1]]))
  g42 <- do.call("s.class", c(list(dfxy = substitute(x$msY), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]][[2]]))
  g43 <- do.call("s.match", c(list(dfxy1 = g41@stats$means, dfxy2 = g42@stats$means, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]][[3]]))
  g4 <- do.call("superpose", list(g41, g42))
  g4@Call <- call("superpose", g41@Call, g42@Call)
  g4 <- do.call("superpose", list(g4, g43))
  g4@Call <- call("superpose", g4@Call, g43@Call)
  g5 <- do.call("s.arrow", c(list(dfxy = substitute(x$l1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g6 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6), positions = layout2position(lay), add = matrix(0, ncol = 6, nrow = 6), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.witrlq" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "witrlq"))
    stop("Object of class 'witrlq' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  
  ## sort parameters for each graph
  graphsnames <- c("Rrow", "Qrow", "Rax", "Rloadings", "Qloadings", "Qax", "eig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "R row scores and classes"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Q row scores"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Unconstrained axes (R)"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[4]] <- list(psub = list(text = "R loadings"), plabels = list(cex = 1.25))
  params[[5]] <- list(psub = list(text = "Q loadings"), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "Unconstrained axes (Q)"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[7]] <- list(psub = list(text = "Eigenvalues"))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.class", c(list(dfxy = substitute(x$lsR), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.label", c(list(dfxy = substitute(x$lQ), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aR), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.arrow", c(list(dfxy = substitute(x$l1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  g5 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  g6 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aQ), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g7 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[7]])) 
  
  ## ADEgS creation
  lay <- matrix(c(1, 1, 3, 1, 1, 4, 2, 2, 5, 2, 2, 6, 0, 0, 7), 3, 5)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6, g7), positions = layout2position(lay), add = matrix(0, ncol = 7, nrow = 7), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.dpcoa" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "dpcoa")) 
    stop("Object of class 'dpcoa' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  dfX <- appel$df
  
  ## sort parameters for each graph
  graphsnames <- c("axes", "categories", "categcoll", "collections")
  
  vec <- c(2, 1, 1, 1)
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = vec)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list()
  params[[1]]$l1 <- list(psub = list(text = "Principal axes", position = "topleft"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[1]]$l2 <- list(psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE))
  params[[2]] <- list(psub = list(text = "Categories"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Categories and collections"), ppoints = list(pch = 16, cex = 1.2), plines = list(col = "transparent"), pellipses = list(axes = list(draw = FALSE)), ellipseSize = 1, plabels = list(cex = 1.25))
  if(!is.null(x$RaoDiv))
    params[[4]] <- list(psub = list(text = "Rao Divcs", position = "topleft"))
  else
    params[[4]] <- list(psub = list(text = "Collections", position = "bottomleft"))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g11 <- do.call("s.corcircle", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[1]]))
  g12 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[2]]))
  g1 <- do.call("insert", list(g12@Call, g11@Call, posi = "bottomleft", plot = FALSE, ratio = 0.25, inset = 0))
  g2 <- do.call("s.label", c(list(dfxy = substitute(x$dls), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.distri", c(list(dfxy = substitute(x$dls), dfdistri = substitute(t(dfX)), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  if(!is.null(x$RaoDiv))
    g4 <- do.call("s.value", c(list(dfxy = substitute(x$li), z = substitute(x$RaoDiv), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  else
    g4 <- do.call("s.label", c(list(dfxy = substitute(x$li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  ## ADEgS creation
  
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(matrix(c(1, 2, 3, 4), 2, 2)), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.betdpcoa" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!(inherits(x, "betdpcoa") | inherits(x, "betwitdpcoa"))) 
    stop("Object of class 'betdpcoa' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  dfX <- as.list(eval.parent(appel$x)$call)$df
  
  ## sort parameters for each graph
  graphsnames <- c("axes", "class", "categories", "Xax")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(2, 1, 1, 1))
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list()
  params[[1]]$l1 <- list(psub = list(text = "Principal axes", position = "topleft"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[1]]$l2 <- list(psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE))
  params[[2]] <- list(psub = list(text = "Classes and collections"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Categories and collections"), ppoints = list(pch = 16, cex = 1.2), plines = list(col = "transparent"), pellipses = list(axes = list(draw = FALSE)), ellipseSize = 1, plabels = list(cex = 1.25))
  params[[4]] <- list(psub = list(text = "Unconstrained axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g11 <- do.call("s.corcircle", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[1]]))
  g12 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[2]]))
  g1 <- do.call("insert", list(g12@Call, g11@Call, posi = "bottomleft", plot = FALSE, ratio = 0.25, inset = 0))
  g2 <- do.call("s.class", c(list(dfxy = substitute(x$ls), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.distri", c(list(dfxy = substitute(x$dls), dfdistri = substitute(t(dfX)), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.corcircle", c(list(dfxy = substitute(x$as), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(matrix(c(1, 2, 3, 4), 2, 2)), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.witdpcoa" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "witdpcoa")) 
    stop("Object of class 'witdpcoa' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  dfX <- as.list(eval.parent(appel$x)$call)$df
  
  ## sort parameters for each graph
  graphsnames <- c("axes", "class", "categories", "Xax")  
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(2, 1, 1, 1))
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list()
  params[[1]]$l1 <- list(psub = list(text = "Principal axes", position = "topleft"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[1]]$l2 <- list(psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE))
  params[[2]] <- list(psub = list(text = "Classes and collections"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Categories and collections"), ppoints = list(pch = 16, cex = 1.2), plines = list(col = "transparent"), pellipses = list(axes = list(draw = FALSE)), ellipseSize = 1, plabels = list(cex = 1.25))
  params[[4]] <- list(psub = list(text = "Unconstrained axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g11 <- do.call("s.corcircle", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[1]]))
  g12 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[2]]))
  g1 <- do.call("insert", list(g12@Call, g11@Call, posi = "bottomleft", plot = FALSE, ratio = 0.25, inset = 0))
  g2 <- do.call("s.class", c(list(dfxy = substitute(x$ls), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.distri", c(list(dfxy = substitute(x$dls), dfdistri = substitute(t(dfX)), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.corcircle", c(list(dfxy = substitute(x$as), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(matrix(c(1, 2, 3, 4), 2, 2)), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.betwitdpcoa" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "betwitdpcoa")) 
    stop("Object of class 'betwitdpcoa' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  dfX <- as.list(eval.parent(appel$x)$call)$df
  
  ## sort parameters for each graph
  graphsnames <- c("axes", "class", "categories", "Xax")  
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(2, 1, 1, 1))
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list()
  params[[1]]$l1 <- list(psub = list(text = "Principal axes", position = "topleft"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[1]]$l2 <- list(psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE))
  params[[2]] <- list(psub = list(text = "Classes and collections"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Categories and collections"), ppoints = list(pch = 16, cex = 1.2), plines = list(col = "transparent"), pellipses = list(axes = list(draw = FALSE)), ellipseSize = 1, plabels = list(cex = 1.25))
  params[[4]] <- list(psub = list(text = "Unconstrained axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g11 <- do.call("s.corcircle", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[1]]))
  g12 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[2]]))
  g1 <- do.call("insert", list(g12@Call, g11@Call, posi = "bottomleft", plot = FALSE, ratio = 0.25, inset = 0))
  g2 <- do.call("s.class", c(list(dfxy = substitute(x$ls), fac = appel$fac, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.distri", c(list(dfxy = substitute(x$dls), dfdistri = substitute(t(dfX)), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.corcircle", c(list(dfxy = substitute(x$as), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(matrix(c(1, 2, 3, 4), 2, 2)), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.mcoa" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "mcoa")) 
    stop("Object of class 'mcoa' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  ## prepare - TODO find better Call for rownames and colnames 
  coolig <- call("as.data.frame", call("matrix", call("kronecker", rep(1, nrow(x$cov2)), substitute(as.matrix(x$SynVar))), nrow = nrow(x$Tl1), ncol = ncol(x$Tl1), dimnames = list(rownames(x$Tl1), colnames(x$Tl1))))
  
  ## sort parameters for each graph
  graphsnames <- c("row", "axes", "col", "pseudoeig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(2, 2, 1, 1))
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list()
  params[[1]]$l1 <- list(psub = list(text = "Rows"), parrows = list(angle = 0), plabels = list(alpha = 0, boxes = list(draw = FALSE)))
  params[[1]]$l2 <- list(plabels = list(cex = 1.25))
  params[[2]] <- list()
  params[[2]]$l1 <- list(psub = list(text = "Axes (separate analyses)", position = "topleft"), pbackground = list(box = FALSE), fullcircle = FALSE, plabels = list(cex = 1.25))
  params[[2]]$l2 <- list(psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE))
  params[[3]] <- list(psub = list(text = "Columns"), plabels = list(cex = 1.25))
  params[[4]] <- list(porigin = list(include = FALSE), paxes = list(aspectratio = "fill", draw = TRUE), main = "Pseudo eigenvalues", xlab = "cov21", ylab = "cov22", plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g11 <- do.call("s.match", c(list(dfxy1 = substitute(x$Tl1), dfxy2 = coolig, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[1]]))
  g12 <- do.call("s.label", c(list(dfxy = substitute(x$SynVar), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]][[2]]))
  g1 <- do.call("superpose", list(g11, g12))
  g1@Call <- call("superpose", g11@Call, g12@Call)
  g21 <- do.call("s.corcircle", c(list(dfxy = substitute(x$Tax[x$T4[, 2] == 1, ]), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]][[1]]))
  g22 <- do.call("plotEig", c(list(eigvalue = substitute(x$pseudoeig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]][[2]]))
  g2 <- do.call("insert", list(g22@Call, g21@Call, posi = "bottomleft", plot = FALSE, ratio = 0.25, inset = 0))
  g3 <- do.call("s.arrow", c(list(dfxy = substitute(x$Tco), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.label", c(list(dfxy = substitute(x$cov2), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4), 2, 2)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(lay), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.foucart" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "foucart"))
    stop("Object of class 'foucart' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("rowB", "colB", "row", "col")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## compute limits
  df <- rbind(as.matrix(x$li), as.matrix(x$Tli), as.matrix(x$Tco))
  adegtot <- adegpar()
  lim.global <- setlimits2D(minX = min(df[, xax]), maxX = max(df[, xax]), minY = min(df[, yax]), maxY = max(df[, yax]), origin = adegtot$porigin$origin, aspect.ratio = adegtot$paxes$aspectratio, includeOr = adegtot$porigin$include)
  
  ## pdefault values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Rows (Base)"), xlim = lim.global$xlim, ylim = lim.global$ylim, plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Columns (Base)"), xlim = lim.global$xlim, ylim = lim.global$ylim, plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Rows"), xlim = lim.global$xlim, ylim = lim.global$ylim, pellipses = list(axes = list(draw = FALSE)))
  params[[4]] <- list(psub = list(text = "Columns"), xlim = lim.global$xlim, ylim = lim.global$ylim, pellipses = list(axes = list(draw = FALSE)), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.label", c(list(dfxy = substitute(x$li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.label", c(list(dfxy = substitute(x$co), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.class", c(list(dfxy = substitute(x$Tli), fac = substitute(x$TL[, 2]), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.class", c(list(dfxy = substitute(x$Tco), fac = substitute(x$TC[, 2]), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 3, 2, 4), 2, 2)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(lay), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  names(object) <- graphsnames
  if(plot) 
    print(object)
  invisible(object)  
}


"plot.mfa" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "mfa")) 
    stop("Object of class 'mfa' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("row", "comp", "col", "link")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(1, 2, 1, 1))
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Rows"), pellipses = list(alpha = 0, axes = list(draw = FALSE)), label = row.names(x$li), plabels = list(cex = 1.25))
  params[[2]] <- list()
  params[[2]]$l1 <- list(psub = list(text = "Components (separate analyses)", position = "topleft"), pbackground = list(box = FALSE), fullcircle = FALSE, plabels = list(cex = 1.25))
  params[[2]]$l2 <- list(psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE))
  params[[3]] <- list(psub = list(text = "Columns"), plabels = list(cex = 1.25))
  params[[4]] <- list(porigin = list(include = FALSE), paxes = list(aspectratio = "fill", draw = TRUE), main = "Link", xlab = "Comp1", ylab = "Comp2", plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.class", c(list(dfxy = substitute(x$lisup), fac = substitute(as.factor(x$TL[, 2])), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g21 <- do.call("s.corcircle", c(list(dfxy = substitute(x$T4comp[x$T4[, 2] == 1, ]), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]][[1]]))
  g22 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]][[2]]))
  g2 <- do.call("insert", list(g22@Call, g21@Call, posi = "bottomleft", plot = FALSE, inset = 0, ratio = 0.2))
  g3 <- do.call("s.arrow", c(list(dfxy = substitute(x$co), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.label", c(list(dfxy = substitute(x$link), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4), 2, 2)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(lay), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.multispati" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "multispati")) 
    stop("Object of class 'multispati' expected")
  if((xax == yax) || ((x$nfposi + x$nfnega) == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > (x$nfposi + x$nfnega)) 
    stop("Non convenient xax")
  if(yax > (x$nfposi + x$nfnega)) 
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("row", "eig", "loadings", "Xax")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Scores and lag scores"))
  params[[2]] <- list(psub = list(text = "Eigenvalues"), paxes = list(draw = TRUE, x = list(draw = FALSE), y = list(draw = TRUE)))
  params[[3]] <- list(psub = list(text = "Loadings"))
  params[[4]] <- list(psub = list(text = "Unconstrained axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.match", c(list(dfxy1 = substitute(x$li), dfxy2 = substitute(x$ls), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = c(1:x$nfposi, length(x$eig):(length(x$eig) - x$nfnega + 1)), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.corcircle",c(list(dfxy = substitute(x$as), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  ## ADEgS creation
  lay <-  matrix(c(rep(0, 4), 2, 2, rep(1, 4), 2, 2, rep(1, 4), 3, 3, rep(1, 4), 3, 3, rep(1, 4), 4, 4, rep(0, 4), 4, 4), 6, 6) 
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(lay), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)   
}


"plot.niche" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "niche")) 
    stop("Object of class 'niche' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("Xax", "var", "eig", "species", "samples", "niches")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(1, 1, 1, 2, 1, 1))
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Unconstrained axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Variables"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list()
  params[[4]]$l1 <- list(psub = list(text = "Samples and Species"), plabels = list(alpha = 0, boxes = list(draw = FALSE)))
  params[[4]]$l2 <- list(plabels = list(cex = 1.25))
  params[[5]] <- list(psub = list(text = "Samples"))
  params[[6]] <- list(psub = list(text = "Niches"), plines = list(col = "transparent"), pellipses = list(axes = list(draw = FALSE)), ellipseSize = 1, plabels = list(alpha = 0, boxes = list(draw = FALSE)))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.corcircle", c(list(dfxy = substitute(x$as), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]])) 
  g41 <- do.call("s.label", c(list(dfxy = substitute(x$ls), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]][[1]]))
  g42 <- do.call("s.label", c(list(dfxy = substitute(x$li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]][[2]]))
  g4 <- do.call("superpose", list(g41, g42))
  g4@Call <- call("superpose", g41@Call, g42@Call)
  g5 <- do.call("s.label", c(list(dfxy = substitute(x$ls), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g6 <- do.call("s.distri", c(list(dfxy = substitute(x$ls), dfdistri = as.list(x$call)[[3]], xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6), positions = layout2position(lay), add = matrix(0, ncol = 6, nrow = 6), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.procuste" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "procuste")) 
    stop("Object of class 'procuste' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("Xloadings", "Yloadings", "eig", "XYmatch", "Xrow", "Yrow")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "X loadings"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Y loadings"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list(psub = list(text = "Row scores (X -> Y)"))
  params[[5]] <- list(psub = list(text = "X row scores"))
  params[[6]] <- list(psub = list(text = "Y row scores"))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.arrow", c(list(dfxy = substitute(x$loadX), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.arrow", c(list(dfxy = substitute(x$loadY), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$d^2), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]])) 
  g4 <- do.call("s.match", c(list(dfxy1 = substitute(x$scorX), dfxy2 = substitute(x$scorY), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  g5 <- do.call("s.label", c(list(dfxy = substitute(x$scorX), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g6 <- do.call("s.label", c(list(dfxy = substitute(x$scorY), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4, 4, 5, 4, 4, 6), 3, 3)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5, g6), positions = layout2position(lay), add = matrix(0, ncol = 6, nrow = 6), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.rlq" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "rlq")) 
    stop("Object of class 'rlq' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  appel <- as.list(x$call)
  
  ## sort parameters for each graph
  graphsnames <- c("Rrow", "Qrow", "Rax", "Rloadings","Qloadings", "Qax", "eig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "R row scores"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Q row scores"), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Unconstrained axes (R)"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[4]] <- list(psub = list(text = "R loadings"))
  params[[5]] <- list(psub = list(text = "Unconstrained axes (Q)"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "Q loadings"))
  params[[7]] <- list(psub = list(text = "Eigenvalues"))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.label", c(list(dfxy = substitute(x$lR), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.label", c(list(dfxy = substitute(x$lQ), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aR), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.arrow", c(list(dfxy = substitute(x$l1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  g5 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aQ), xax, yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  g6 <- do.call("s.arrow", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[6]]))
  g7 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[7]])) 
  
  ## ADEgS creation
  lay <- matrix(c(1, 1, 3, 1, 1, 4, 2, 2, 5, 2, 2, 6, 0, 0, 7), 3, 5)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g6, g5, g7), positions = layout2position(lay), add = matrix(0, ncol = 7, nrow = 7), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.pta" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "pta")) 
    stop("Object of class 'pta' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  ## prepare
  dfxy <- substitute(matrix(c(x$tabw, x$cos2), nrow = length(x$tabw), ncol = 2, dimnames = list(rownames(x$RV))))
  
  ## sort parameters for each graph
  graphsnames <- c("inter", "col", "row", "typo")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(2, 1, 2, 1))
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list()
  params[[1]]$l1 <- list(psub = list(text = "Interstructure", position = "topleft"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[1]]$l2 <- list(psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE), p1d = list(horizontal = FALSE))
  params[[2]] <- list(psub = list(text = "Columns (compromise)", position = "topleft"), plabels = list(cex = 1.25))
  params[[3]] <- list()
  params[[3]]$l1 <- list(psub = list(text = "Rows (compromise)", position = "topleft"), plabels = list(cex = 1.25))
  params[[3]]$l2 <- list(psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE), p1d = list(horizontal = FALSE))
  params[[4]] <- list(porigin = list(include = FALSE), paxes = list(aspectratio = "fill", draw = TRUE), main = "Typological value", xlab = "Tables weights", ylab = "Cos 2", plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g11 <- do.call("s.corcircle", c(list(dfxy = substitute(x$RV.coo), xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]$l1))
  g12 <- do.call("plotEig", c(list(eigvalue = substitute(x$RV.eig), nf = 1:length(x$RV.eig), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]$l2))
  g1 <- do.call("insert", list(g12@Call, g11@Call, posi = "bottomleft", plot = FALSE, ratio = 0.25, inset = 0))
  g2 <- do.call("s.arrow", c(list(dfxy = substitute(x$co), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g31 <- do.call("s.label", c(list(dfxy = substitute(x$li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]$l1))
  g32 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]$l2))
  g3 <- do.call("insert", list(g32@Call, g31@Call, posi = "bottomleft", plot = FALSE, ratio = 0.25, inset = 0))
  g4 <- do.call("s.label", c(list(dfxy = dfxy, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4), 2, 2)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(lay), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.sepan" <- function(x, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "sepan")) 
    stop("Object of class 'sepan' expected")
  
  ## prepare 
  facets <- substitute(reorder(as.factor(rep(x$tab.names, x$rank)), rep(1:length(x$rank), x$rank)))
  
  ## default values for parameters
  sortparameters <- sortparamADEg(...)
  params <- list()
  params$adepar <- list(pbackground = list(box = TRUE), pgrid = list(draw = TRUE, text = list(cex = 0)), paxes = list(draw = TRUE, x = list(draw = FALSE)))
  if(isTRUE(sortparameters$adepar$p1d$horizontal))
    params$g.args <- list(ylim = c(0, max(x$rank) + 1))
  else
    params$g.args <- list(xlim = c(0, max(x$rank) + 1))
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## ADEgS creation
  object <- do.call("plotEig", c(list(eigvalue = substitute(x$Eig), nf = 1:ncol(x$Li), xax = 1, yax = 2, pos = pos, storeData = storeData, plot = FALSE, facets = facets), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args))
  object@Call <- match.call()
  if(plot)
    print(object)
  invisible(object)
}


"plot.statis" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "statis")) 
    stop("Object of class 'statis' expected")
  if((xax == yax) || (x$C.nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$C.nf) 
    stop("Non convenient xax")
  if(yax > x$C.nf) 
    stop("Non convenient yax")
  
  ## prepare
  dfxy <- substitute(matrix(c(x$RV.tabw, x$cos2), nrow = length(x$RV.tabw), ncol = 2, dimnames = list(rownames(x$RV))))
  
  ## sort parameters for each graph
  graphsnames <- c("inter", "typo", "row", "comp")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames, nbsubgraphs = c(1, 1, 2, 1))
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(psub = list(text = "Interstructure", position = "topleft"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[2]] <- list(porigin = list(include = FALSE), paxes = list(aspectratio = "fill", draw = TRUE), main = "Typological Value", xlab = "Tables Weights", ylab = "Cos 2", plabels = list(cex = 1.25))
  params[[3]] <- list()
  params[[3]]$l1 <- list(psub = list(text = "Rows (compromise)", position = "topleft"), plabels = list(cex = 1.25))
  params[[3]]$l2 <- list(psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE))
  params[[4]] <- list(psub = list(text = "Components (separate analyses)", position = "topleft"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.corcircle", c(list(dfxy = substitute(x$RV.coo), xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.label", c(list(dfxy = dfxy, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g31 <- do.call("s.label", c(list(dfxy = substitute(x$C.li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]][[1]]))
  g32 <- do.call("plotEig", c(list(eigvalue = substitute(x$C.eig), nf = 1:x$C.nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData), sortparameters[[3]][[2]]))
  g3 <- do.call("insert", list(g32@Call, g31@Call, posi = "bottomleft", plot = FALSE, ratio = 0.25, inset = 0))
  g4 <- do.call("s.corcircle", c(list(dfxy = substitute(x$C.T4[x$T4[, 2] == 1, ]), xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  ## ADEgS creation
  lay <- matrix(c(1, 2, 3, 4), 2, 2)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = layout2position(lay), add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.multiblock" <- function(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "multiblock")) 
    stop("Object of class 'multiblock' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf) 
    stop("Non convenient xax")
  if(yax > x$nf) 
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("Xrow", "eig", "cov2", "Ycol", "Xloadings")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## default values for parameters
  params <- list()
  params[[1]]<- list(psub = list(text = "Row scores (X)"), plabels = list(cex = 1.25))
  params[[2]]<- list(psub = list(text = "Eigenvalues"))
  params[[3]] <- list(psub = list(text = "Cov^2"), plabels = list(cex = 1.25))
  params[[4]] <- list(psub = list(text = "Y columns"), plabels = list(cex = 1.25))
  params[[5]] <- list(psub = list(text = "X loadings"), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.label", c(list(dfxy = substitute(x$lX), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s.arrow", c(list(dfxy = substitute(x$cov2), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s.arrow", c(list(dfxy = substitute(x$Yco), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  g5 <- do.call("s.arrow", c(list(dfxy = substitute(x$faX), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[5]]))
  
  ## ADEgS creation
  lay <- matrix(c(rep(c(0, 0, 2, 2, 3, 3), 2), rep(c(rep(1, 4), 4, 4), 2), rep(c(rep(1, 4), 5, 5), 2)), 6, 6)
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4, g5), positions = layout2position(lay), add = matrix(0, ncol = 5, nrow = 5), Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.randxval" <- function(x, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "randxval")) 
    stop("Object of class 'randxval' expected")
  
  ## Plot results 
  graphsnames <- c("RMSEcMean", "RMSEcQuantiles", "RMSEvMean", "RMSEvQuantiles")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## compute common limits
  lim <- range(x$stats)
  origin <- if(is.null(sortparameters[[1]]$porigin)) list(origin = 0, include = FALSE) else sortparameters[[1]]$porigin
  lim <- setlimits1D(lim[1], lim[2], origin = origin$origin[1], includeOr = origin$include)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(plines.col = "red", ppoints.col = "red", p1d.horizontal = FALSE, paxes.draw = TRUE, ppoints.cex = 2, ylab = "Root Mean Square Error", ylim = lim, porigin = origin)
  params[[2]] <- list(plines.col = "red", ppoly.col = "red", p1d.horizontal = FALSE, paxes.draw = TRUE, method = "bars")
  params[[3]] <- list(plines.col = "blue", ppoints.col = "blue", p1d.horizontal = FALSE, paxes.draw = TRUE, ppoints.cex = 2)
  params[[4]] <- list(plines.col = "blue", ppoly.col = "blue", p1d.horizontal = FALSE, paxes.draw = TRUE, method = "bars")
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s1d.curve", c(list(score = substitute(x$stats[1, 1]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s1d.interval", c(list(score1 = substitute(x$stats[1, 2]), score2 = substitute(x$stats[1, 3]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s1d.curve", c(list(score = substitute(x$stats[2, 1]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s1d.interval", c(list(score1 = substitute(x$stats[2, 2]), score2 = substitute(x$stats[2, 3]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  ## ADEgS creation
  add.mat <- matrix(0, nrow = 4, ncol = 4)
  add.mat[upper.tri(add.mat)] <- 1
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = matrix(rep(c(0, 0, 1, 1), 4), nrow = 4, byrow = TRUE), add = add.mat, Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.krandxval" <- function(x, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "krandxval")) 
    stop("Object of class 'krandxval' expected")
  
  ## Plot results 
  graphsnames <- c("RMSEcMean", "RMSEcQuantiles", "RMSEvMean", "RMSEvQuantiles")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## compute common limits
  lim <- range(x$statsRMSEc[, -1], x$statsRMSEv[, -1])
  origin <- if(is.null(sortparameters[[1]]$porigin)) list(origin = 0, include = FALSE) else sortparameters[[1]]$porigin
  lim <- setlimits1D(lim[1], lim[2], origin = origin$origin[1], includeOr = origin$include)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(plines.col = "red", ppoints.col = "red", p1d.horizontal = FALSE, paxes.draw = TRUE, ppoints.cex = 2, ylab = "Root Mean Square Error", ylim = lim, porigin = origin)
  params[[2]] <- list(plines.col = "red", ppoly.col = "red", p1d.horizontal = FALSE, paxes.draw = TRUE, method = "area")
  params[[3]] <- list(plines.col = "blue", ppoints.col = "blue", p1d.horizontal = FALSE, paxes.draw = TRUE, ppoints.cex = 2)
  params[[4]] <- list(plines.col = "blue", ppoly.col = "blue", p1d.horizontal = FALSE, paxes.draw = TRUE, method = "area")
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s1d.curve", c(list(score = substitute(x$statsRMSEc[, 1]), key = list(corner = c(0,1), text = list(c("RMSEc", "RMSEv"), col = c(sortparameters[[1]]$plines.col, sortparameters[[3]]$plines.col))), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s1d.interval", c(list(score1 = substitute(x$statsRMSEc[, 2]), score2 = substitute(x$statsRMSEc[, 3]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call("s1d.curve", c(list(score = substitute(x$statsRMSEv[, 1]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[3]]))
  g4 <- do.call("s1d.interval", c(list(score1 = substitute(x$statsRMSEv[, 2]), score2 = substitute(x$statsRMSEv[, 3]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[4]]))
  
  ## ADEgS creation
  add.mat <- matrix(0, nrow = 4, ncol = 4)
  add.mat[upper.tri(add.mat)] <- 1
  object <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = matrix(rep(c(0, 0, 1, 1), 4), nrow = 4, byrow = TRUE), add = add.mat, Call = match.call())
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.randboot" <- function(x, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "randboot")) 
    stop("Object of class 'randboot' expected")
  
  ## Plot results 
  graphsnames <- c("obs", "quantiles")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## compute common limits
  lim <- range(c(x$obs, x$stats))
  origin <- if(is.null(sortparameters[[1]]$porigin)) list(origin = 0, include = FALSE) else sortparameters[[1]]$porigin
  lim <- setlimits1D(lim[1], lim[2], origin = origin$origin[1], includeOr = origin$include)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(p1d.horizontal = FALSE, paxes.draw = TRUE, ppoints.cex = 2, ylim = lim, porigin = origin)
  params[[2]] <- list(p1d.horizontal = FALSE, paxes.draw = TRUE, method = "bars")
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s1d.curve", c(list(score = substitute(x$obs), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s1d.interval", c(list(score1 = substitute(x$stats[1]), score2 = substitute(x$stats[2]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  
  ## ADEgS creation
  object <- superpose(g1, g2)
  object@Call <- match.call()
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.krandboot" <- function(x, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "krandboot")) 
    stop("Object of class 'krandboot' expected")
  
  ## Plot results 
  graphsnames <- c("obs", "quantiles")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## compute common limits
  lim <- range(c(x$obs, range(x$stats)))
  origin <- if(is.null(sortparameters[[1]]$porigin)) list(origin = 0, include = FALSE) else sortparameters[[1]]$porigin
  lim <- setlimits1D(lim[1], lim[2], origin = origin$origin[1], includeOr = origin$include)
  
  ## default values for parameters
  params <- list()
  params[[1]] <- list(p1d.horizontal = FALSE, paxes.draw = TRUE, ppoints.cex = 2, ylim = lim, porigin = origin)
  params[[2]] <- list(p1d.horizontal = FALSE, paxes.draw = TRUE, method = "bars")
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  lab <- list(list(labels = rownames(x$stats), rot = 90))
  names(lab) <- ifelse(sortparameters[[1]]$p1d.horizontal == FALSE, "x", "y")
  ## creation of each individual ADEg
  g1 <- do.call("s1d.curve", c(list(score = substitute(x$obs), scales = lab, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s1d.interval", c(list(score1 = substitute(x$stats[, 1]), score2 = substitute(x$stats[, 2]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  
  ## ADEgS creation
  object <- superpose(g1, g2)
  object@Call <- match.call()
  names(object) <- graphsnames
  if(plot)
    print(object)
  invisible(object)
}


"plot.inertia" <- function(x, xax = 1, yax = 2, cont = 0.1, type = c("label", "cross", "ellipse", "both"), ellipseSize = 1.5, 
                           posieig = "none", plot = TRUE, storeData = TRUE, pos = -1, ...) {
  
  if(!inherits(x, "inertia")) 
    stop("Object of class 'inertia' expected")
  
  ## data management
  ori <- as.list(x$call)
  evTab <- eval.parent(ori[[2]])
  
  if(length(xax) > 1)
    stop("Not implemented for multiple xax")
  if(xax > evTab$nf)
    stop("Non convenient xax")
  
  if(length(yax) > 1)
    stop("Not implemented for multiple yax")
  if(yax > evTab$nf)
    stop("Non convenient yax")
  
  adegtot <- adegpar()
  position <- match.arg(posieig[1], choices = c("bottomleft", "bottomright", "topleft", "topright", "none"), several.ok = FALSE)
  type <- match.arg(type)
  type <- type[1]
  
  ## sort parameters for each graph
  graphsnames <- c("light_row", "heavy_row", "light_col", "heavy_col", "eig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  adegtot <- adegpar()
  params <- list()
  params$light_row <- list(plabels = list(cex = 0), ppoints = list(col = "grey20", alpha = 0.45, cex = 1.2, pch = 19))
  params$light_col <- list(plabels = list(cex = 0), ppoints = list(col = "grey20", alpha = 0.45, cex = 1.2, pch = 19))
  
  if(type == "label") {
    params$heavy_row <- list(plabels = list(boxes = list(draw = FALSE), col = "red"), ppoints = list(cex = 0))
    params$heavy_col <- list(plabels = list(boxes = list(draw = FALSE), col = "blue"), ppoints = list(cex = 0))
  } else if(type == "cross") {
    params$heavy_row <- list(ellipseSize = ellipseSize, plabels = list(boxes = list(draw = FALSE), col = "red"), ppoints = list(cex = 0), pellipses = list(lwd = 0, axes = list(col = "red", lty = 1)), plines = list(lwd = 0), plegend = list(drawKey = FALSE))
    params$heavy_col <- list(ellipseSize = ellipseSize, plabels = list(boxes = list(draw = FALSE), col = "blue"), ppoints = list(cex = 0), pellipses = list(lwd = 0, axes = list(col = "blue", lty = 1)), plines = list(lwd = 0), plegend = list(drawKey = FALSE))
  } else if(type == "ellipse") {
    params$heavy_row <- list(ellipseSize = ellipseSize, plabels = list(boxes = list(draw = FALSE), col = "red"), ppoints = list(cex = 0), pellipses = list(border = "red", axes = list(lwd = 0)), plines = list(col = 0))
    params$heavy_col <- list(ellipseSize = ellipseSize, plabels = list(boxes = list(draw = FALSE), col = "blue"), ppoints = list(cex = 0), pellipses = list(border = "blue", axes = list(lwd = 0)), plines = list(lwd = 0))
  } else if(type == "both") {
    params$heavy_row <- list(ellipseSize = 1.5, plabels = list(boxes = list(draw = FALSE), col = "red"), ppoints = list(cex = 0), pellipses = list(border = "red", axes = list(col = "red", lty = 1)), plines = list(lwd = 0))
    params$heavy_col <- list(ellipseSize = 1.5, plabels = list(boxes = list(draw = FALSE), col = "blue"), ppoints = list(cex = 0), pellipses = list(border = "blue", axes = list(col = "blue", lty = 1)), plines = list(lwd = 0))
  }
  params$eig <- list(pbackground = list(box = TRUE), psub = list(text = "Eigenvalues"))
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## management of the data and the parameters about the rows' contribution (individuals) on axes
  if(!is.null(x$row.abs)) {
    inertrow <- x$row.abs[, c(xax, yax)] / 100
    inertrowcall <- call("/", call("[", call("$", substitute(x), "row.abs"), call(":", 1, call("NROW", call("$", substitute(x), "row.abs"))), c(xax, yax)), 100)
    lightrow <- subset(evTab$li[, c(xax, yax)], inertrow[, 1] < cont & inertrow[, 2] < cont)
    lightrowcall <- call("subset", call("[", call("$", ori[[2]], "li"), call(":", 1, call("NROW", call("$", ori[[2]], "li"))), c(xax, yax)), call("&", call("<", call("[", inertrowcall, 1), cont), call("<", call("[", inertrowcall, 2), cont)))
    
    heavyrow <- subset(evTab$li[, c(xax, yax)], inertrow[, 1] >= cont | inertrow[, 2] >= cont)
    heavyrowcall <- call("subset", call("[", call("$", ori[[2]], "li"), call(":", 1, call("NROW", call("$", ori[[2]], "li"))), c(xax, yax)), call("&", call(">=", call("[", inertrowcall, 1), cont), call(">=", call("[", inertrowcall, 2), cont)))
    if(nrow(heavyrow) == 0)
      stop("No points to draw, try lowering 'cont'")
    heavy_inertrow <- subset(inertrow, inertrow[, 1] >= cont | inertrow[, 2] >= cont)
    heavy_inertrowcum <- apply(heavy_inertrow, 1, sum)
    
    if(type == "label"){
      if(is.null(sortparameters$heavy_row$plabels$cex)) {
        sortparameters$heavy_row$plabels$cex <- heavy_inertrowcum / (max(heavy_inertrowcum) / 1.5)
      } else {
        sortparameters$heavy_row$plabels$cex <- sortparameters$heavy_row$plabels$cex * heavy_inertrowcum / (max(heavy_inertrowcum) / 1.5)
      }
    }
    
    limglobal <- setlimits2D(minX = min(c(heavyrow[, 1], lightrow[, 1])), maxX = max(c(heavyrow[, 1], lightrow[, 1])), 
                             minY = min(c(heavyrow[, 2], lightrow[, 2])), maxY = max(c(heavyrow[, 2], lightrow[, 2])),
                             origin = adegtot$porigin$origin, aspect.ratio = adegtot$paxes$aspectratio, includeOr = adegtot$porigin$include)
    
    if(type != "label") {
      # if ellipses or crosses are drawn, the limits are re-calculated and the elipses size are normalized
      heavy_inertrowmax <- apply(heavy_inertrow, 2, max)
      heavy_inertrownorm <- matrix(NA, NROW(heavy_inertrow), 2)
      for (i in 1:2) {heavy_inertrownorm[, i] <- (heavy_inertrow[, i] / heavy_inertrowmax[i]) * (diff(limglobal[[i]]) / 10)}
      
      # TODO
      # add 0.00001 to the coordinates to avoid the bug in the '.util.ellipse' function (waiting to correct it)
      cont_row <- cbind(c(heavyrow[, 1] - heavy_inertrownorm[, 1]/2, heavyrow[, 1] + heavy_inertrownorm[, 1]/2, heavyrow[, 1], heavyrow[, 1] + 0.00001), 
                        c(heavyrow[, 2] + 0.00001, heavyrow[, 2], heavyrow[, 2] - heavy_inertrownorm[, 2]/2, heavyrow[, 2] + heavy_inertrownorm[, 2]/2)) 
      fac_row <- as.factor(rep(rownames(heavyrow), 4))
      limglobal <- setlimits2D(minX = min(c(cont_row[, 1], lightrow[, 1])), maxX = max(c(cont_row[, 1], lightrow[, 1])), 
                               minY = min(c(cont_row[, 2], lightrow[, 2])), maxY = max(c(cont_row[, 2], lightrow[, 2])),
                               origin = adegtot$porigin$origin, aspect.ratio = adegtot$paxes$aspectratio, includeOr = adegtot$porigin$include)
    }
    
    params <- list()
    params$light_row <- list(xlim = limglobal$xlim, ylim = limglobal$ylim)
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  }
  
  ## management of the data and the parameters about the columns' contribution (variables) on axes
  if(!is.null(x$col.abs)) {
    inertcol <- x$col.abs[, c(xax, yax)] / 100
    inertcolcall <- call("/", call("[", call("$", substitute(x), "col.abs"), call(":", 1, call("NROW", call("$", substitute(x), "col.abs"))), c(xax, yax)), 100)
    lightcol <- subset(evTab$co[, c(xax, yax)], inertcol[, 1] < cont & inertcol[, 2] < cont)
    lightcolcall <- call("subset", call("[", call("$", ori[[2]], "co"), call(":", 1, call("NROW", call("$", ori[[2]], "co"))), c(xax, yax)), call("&", call("<", call("[", inertcolcall, 1), cont), call("<", call("[", inertcolcall, 2), cont)))
    
    heavycol <- subset(evTab$co[, c(xax, yax)], inertcol[, 1] >= cont | inertcol[, 2] >= cont)
    heavycolcall <- call("subset", call("[", call("$", ori[[2]], "co"), call(":", 1, call("NROW", call("$", ori[[2]], "co"))), c(xax, yax)), call("&", call(">=", call("[", inertcolcall, 1), cont), call(">=", call("[", inertcolcall, 2), cont)))
    if(nrow(heavycol) == 0)
      stop("No points to draw, try lowering 'cont'")
    heavy_inertcol <- subset(inertcol, inertcol[, 1] >= cont | inertcol[, 2] >= cont)
    heavy_inertcolcum <- apply(heavy_inertcol, 1, sum)
    
    if(type == "label") {
      if(is.null(sortparameters$heavy_col$plabels$cex)) {
        sortparameters$heavy_col$plabels$cex <- heavy_inertcolcum / (max(heavy_inertcolcum) / 1.5)
      } else {
        sortparameters$heavy_col$plabels$cex <- sortparameters$heavy_col$plabels$cex * heavy_inertcolcum / (max(heavy_inertcolcum) / 1.5)
      }
    }
    
    limglobal <- setlimits2D(minX = min(c(heavycol[, 1], lightcol[, 1])), maxX = max(c(heavycol[, 1], lightcol[, 1])), 
                             minY = min(c(heavycol[, 2], lightcol[, 2])), maxY = max(c(heavycol[, 2], lightcol[, 2])),
                             origin = adegtot$porigin$origin, aspect.ratio = adegtot$paxes$aspectratio, includeOr = adegtot$porigin$include)
    
    if(type != "label") {
      # if ellipses or crosses are drawn, the limits are re-calculated and the ellipse size are normalized
      heavy_inertcolmax <- apply(heavy_inertcol, 2, max)
      heavy_inertcolnorm <- matrix(NA, NROW(heavy_inertcol), 2)
      for (i in 1:2) {heavy_inertcolnorm[, i] <- (heavy_inertcol[, i] / heavy_inertcolmax[i]) * (diff(limglobal[[i]]) / 10)}
      
      # TODO
      # add 0.00001 to the coordinates to avoid the bug in the '.util.ellipse' function (waiting to correct it)
      cont_col <- cbind(c(heavycol[, 1] - heavy_inertcolnorm[, 1]/2, heavycol[, 1] + heavy_inertcolnorm[, 1]/2, heavycol[, 1], heavycol[, 1] + 0.00001), 
                        c(heavycol[, 2] + 0.00001, heavycol[, 2], heavycol[, 2] - heavy_inertcolnorm[, 2]/2, heavycol[, 2] + heavy_inertcolnorm[, 2]/2))  
      fac_col <- as.factor(rep(rownames(heavycol), 4))
      limglobal <- setlimits2D(minX = min(c(cont_col[, 1], lightcol[, 1])), maxX = max(c(cont_col[, 1], lightcol[, 1])), 
                               minY = min(c(cont_col[, 2], lightcol[, 2])), maxY = max(c(cont_col[, 2], lightcol[, 2])),
                               origin = adegtot$porigin$origin, aspect.ratio = adegtot$paxes$aspectratio, includeOr = adegtot$porigin$include)
    }
    
    params <- list()
    params$light_col <- list(xlim = limglobal$xlim, ylim = limglobal$ylim)
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  }
  
  
  ## displaying of the eigen values
  if(position != "none")
    geig <- do.call("plotEig", c(list(eigvalue = call("$", ori[[2]], "eig"), nf = 1:evTab$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$eig))
  
  ## function to create the graphics about the row' contribution (individuals) on axes
  f_row <- function(posi = "none", pos){
    graphnames <- c(if(length(lightrow) > 0) {"light_row"}, "heavy_row", if(posi != "none") {"eig"})
    
    g1 <- do.call("s.label", c(list(dfxy = lightrow, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$light_row))
    if(type == "label")
      g2 <- do.call("s.label", c(list(dfxy = heavyrow, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$heavy_row))
    else
      g2 <- do.call("s.class", c(list(dfxy = cont_row, fac = fac_row, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$heavy_row))
    grow <- do.call("superpose", list(g1, g2))
    grow@Call <- call("superpose", list(g1@Call, g2@Call))
    
    if(posi != "none")
      grow <- do.call("insert", list(geig, grow, posi = posi, plot = FALSE, ratio = 0.25))
    names(grow) <- graphnames
    return(grow)
  }
  
  # function to create the graphics about the columns' contribution (variables) on axes
  f_col <- function(posi = "none", pos) {
    graphnames <- c(if(length(lightcol) > 0) {"light_col"}, "heavy_col", if(posi != "none") {"eig"})
    
    g3 <- do.call("s.label", c(list(dfxy = lightcol, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$light_col))
    if(type == "label")
      g4 <- do.call("s.label", c(list(dfxy = heavycol, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$heavy_col))
    else
      g4 <- do.call("s.class", c(list(dfxy = cont_col, fac = fac_col, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$heavy_col))
    gcol <- do.call("superpose", list(g3, g4))
    gcol@Call <- call("superpose", list(g3@Call, g4@Call))
    
    if(posi != "none")
      gcol <- do.call("insert", list(geig, gcol, posi = posi, plot = FALSE, ratio = 0.25))
    names(gcol) <- graphnames
    return(gcol)
  }
  
  ## function to create a layout of the graphics about the contribution of rows (individuals) and columns (variables) on axes
  f_both <- function(posi = "none", pos) {
    object <- do.call("cbindADEg", c(list(f_row(posi = "none", pos = pos - 1), f_col(posi = posi, pos = pos - 1))))
    names(object) <- c("row", "col")
    return(object)
  }
  
  ## creation of the appropriate plot according to the input data
  if(!is.null(x$row.abs) & is.null(x$col.abs))
    object <- f_row(posi = position, pos = pos)
  if(!is.null(x$col.abs) & is.null(x$row.abs))
    object <- f_col(posi = position, pos = pos)
  if(!is.null(x$row.abs) & !is.null(x$col.abs))
    object <- f_both(posi = position, pos = pos)
  if(is.null(x$row.abs) & is.null(x$col.abs))
    stop(paste("No inertia was calculated in the ", substitute(x), " object", sep = ""))
  
  object@Call <- match.call()
  
  if(plot)
    print(object)
  invisible(object)
}
