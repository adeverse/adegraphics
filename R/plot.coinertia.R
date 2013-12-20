"plot.coinertia" <- function(x, xax = 1, yax = 2, pos = -1, storeData = FALSE, plot = TRUE, ...) {
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
  sortparameters <- .paramsADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params[[1]] <- list(psub = list(text = "X axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "Y axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list(psub = list(text = "X -> Y"))
  params[[5]] <- list(psub = list(text = "Y loadings"), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "X loadings"), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## Creation of each individual ADEg
  g1 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aX), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.corcircle", c(list(dfxy = substitute(x$aY), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call(".add.scatter.eig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE), sortparameters[[3]]))
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


"plot.pcaiv" <- function(x, xax = 1, yax = 2, pos = -1, storeData = FALSE, plot = TRUE, ...) {
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
  graphsnames <- c("Xloadings", "Xcor", "eig", "XYmatch", "Yax", "Yvar")
  sortparameters <- .paramsADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params[[1]] <- list(psub = list(text = "X loadings"), plabels = list(cex = 1.25))
  params[[2]] <- list(psub = list(text = "X correlation"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[3]] <- list(psub = list(text = "Eigenvalues"))
  params[[4]] <- list(psub = list(text = "Predictions (X) -> Scores (Y)"))
  params[[5]] <- list(psub = list(text = "Y axes"), pbackground = list(box = FALSE), plabels = list(cex = 1.25))
  params[[6]] <- list(psub = list(text = "Y variables"), plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## Creation of each individual ADEg
  g1 <- do.call("s.arrow", c(list(dfxy = substitute(na.omit(x$fa)), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[1]]))
  g2 <- do.call("s.corcircle", c(list(dfxy = substitute(na.omit(x$cor)), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters[[2]]))
  g3 <- do.call(".add.scatter.eig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE), sortparameters[[3]]))
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
