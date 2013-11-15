"plot.foucart" <- function(x, xax = 1, yax = 2, pos = -1, storeData = FALSE, plot = TRUE, ...) {
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
  graphsnames <- c("Rb1", "Cb1", "R1", "C1")
  sortparameters <- .paramsADEgS(..., graphsnames = graphsnames)
  
	## limits calcul
  df <- rbind(as.matrix(x$li), as.matrix(x$Tli), as.matrix(x$Tco))
  adegtot <- adegpar()
  lim.global <- .setlimits(minX = min(df[, xax]), maxX = max(df[, xax]), minY = min(df[, yax]), maxY = max(df[, yax]), origin = adegtot$porigin$origin, aspect.ratio = adegtot$paxes$aspectratio, includeOr = adegtot$porigin$include)
  
  ## parameters management
  params <- list()
  params[[1]] <- list(psub = list(text = "Rows - Base"), xlim = lim.global$xlim, ylim = lim.global$ylim)
  params[[2]] <- list(psub = list(text = "Columns - Base"), xlim = lim.global$xlim, ylim = lim.global$ylim)
  params[[3]] <- list(psub = list(text = "Rows"), xlim = lim.global$xlim, ylim = lim.global$ylim, pellipses = list(axes = list(draw = FALSE)))
  params[[4]] <- list(psub = list(text = "Columns"), xlim = lim.global$xlim, ylim = lim.global$ylim, pellipses = list(axes = list(draw = FALSE)))
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
