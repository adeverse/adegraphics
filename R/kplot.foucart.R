"kplot.foucart" <- function(object, xax = 1, yax = 2, which.tab = 1:length(object$blo), pos = -1, storeData = FALSE, plot = TRUE, ...) {
  if(!inherits(object, "foucart")) 
	  stop("Object of class 'foucart' expected")
  if((xax == yax) || (object$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > object$nf)
    stop("Non convenient xax")
  if(yax > object$nf)
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("row", "col")
  sortparameters <- .paramsADEgS(..., graphsnames = graphsnames)
  
	## limits calcul
  df <- rbind(as.matrix(object$li), as.matrix(object$Tli), as.matrix(object$Tco))
  adegtot <- adegpar()
  lim.global <- .setlimits(minX = min(df[,xax]), maxX = max(df[,xax]), minY = min(df[ ,yax]), maxY = max(df[ ,yax]), origin = adegtot$porigin$origin, aspect.ratio = adegtot$paxes$aspectratio, includeOr = adegtot$porigin$include)
  
  ## prepare subs (should be in ade4 : TC should contains tab.names instead of numbers)
  ## levels(object$TL[, 1]) <- levels(object$TC[, 1]) <- object$tab.names
  TL <- object$TL[, 1]
  levels(TL) <- object$tab.names
  TC <- object$TC[, 1]
  levels(TC) <- object$tab.names
  
  ## parameters management
  params <- list()
  params$row <- list(plabels = list(cex = 1), xlim = lim.global$xlim, ylim = lim.global$ylim, plabels = list(cex = 1.25))
  params$col <- list(plabels = list(cex = 1.25), psub = list(text = ""), xlim = lim.global$xlim, ylim = lim.global$ylim, plabels = list(cex = 1.25))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  g1 <- do.call("s.label", c(list(dfxy = substitute(object$Tli), facets = TL, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))[which.tab]
  g2 <- do.call("s.label", c(list(dfxy = substitute(object$Tco), facets = TC, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))[which.tab]
  
  ## ADEgS creation
  obj <- do.call("superpose", list(g1, g2))
  names(obj) <- object$tab.names
  obj@Call <- match.call()
  if(plot)
    print(obj)
  invisible(obj)
}
