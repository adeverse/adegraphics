"kplot.mcoa" <- function(object, xax = 1, yax = 2, which.tab = 1:nrow(object$cov2), option = c("points", "axis", "columns"), pos = -1, storeData = FALSE, plot = TRUE, ...) {
  if(!inherits(object, "mcoa")) 
    stop("Object of class 'mcoa' expected")
  if((xax == yax) || (object$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > object$nf)
    stop("Non convenient xax")
  if(yax > object$nf)
    stop("Non convenient yax")
  
  option <- match.arg(option)
  
  ## parameters management
  sortparameters <- .specificpar(...)
  
  if(option == "points") {
    params1 <- list()
    params1$adepar <- list(psub = list(text = "Reference"))
    sortparameters1 <- modifyList(params1, sortparameters, keep.null = TRUE)
    ref <- do.call("s.label", c(list(dfxy = substitute(object$SynVar), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters1$adepar, sortparameters1$trellis, sortparameters1$g.args))
      
    params2 <- list()
    params2$adepar <- list(plabels = list(cex = 0))
    params2$g.args <- list(samelimits = FALSE)
    sortparameters2 <- modifyList(params2, sortparameters, keep.null = TRUE)
    
    facets1 <- substitute(object$TL[,1])
    coolig <- call("as.data.frame", call("matrix", call("kronecker", rep(1,nrow(object$cov2)), substitute(as.matrix(object$SynVar))), nrow = nrow(object$Tl1), ncol = ncol(object$Tl1), dimnames = substitute(list(rownames(object$Tl1), colnames(object$Tl1)))))
    g1 <- do.call("s.match", c(list(dfxy1 = coolig, dfxy2 = substitute(object$Tl1), facets = facets1, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters2$adepar, sortparameters2$trellis, sortparameters2$g.args))[which.tab]
    
    ## ADEgS creation
    ADEglist <- c(list(ref), g1@ADEglist)
    nrow_lay  <- floor(sqrt(length(ADEglist))) + 1
    ncol_lay <- -floor(-(length(ADEglist)) / nrow_lay)
    lay <- matrix(c(seq(1, length(ADEglist)), rep(0, nrow_lay * ncol_lay - length(ADEglist))), nrow = nrow_lay, byrow = TRUE)
    obj <- new(Class = "ADEgS", ADEglist = ADEglist, positions = layout2position(lay), add = matrix(0, ncol = length(ADEglist), nrow = length(ADEglist)), Call = match.call())
    names(obj) <- c("ref", names(g1))
    
  } else if(option == "axis") {
    params <- list()
    params$adepar <- list(pbackground = list(box = FALSE))
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    
    facets2 <- substitute(object$T4[, 1])
    obj <- do.call("s.corcircle", c(list(dfxy = substitute(object$Tax), facets = facets2, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args))[which.tab]
    
  } else if(option == "columns") {
    params <- list()
    params$g.args <- list(samelimits = FALSE)
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    
    facets3 <- substitute(object$TC[, 1])
    obj <- do.call("s.arrow", c(list(dfxy = substitute(object$Tco), facets = facets3, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args))[which.tab]
  }
  
  obj@Call <- match.call()
  if(plot) 
    print(obj)
  invisible(obj)
}


"kplot.mfa" <- function(object, xax = 1, yax = 2, which.tab = 1:length(object$blo), traject = FALSE, permute = FALSE, pos = -1, storeData = FALSE, plot = TRUE, ...) {
  if(!inherits(object, "mfa")) 
    stop("Object of class 'mfa' expected")
  if((xax == yax) || (object$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > object$nf)
    stop("Non convenient xax")
  if(yax > object$nf)
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("row", "col", "traj")
  sortparameters <- .paramsADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params$row <- list(plabels = list(cex = 0), ppoints = list(cex = 1.5), samelimits = FALSE)
  params$col <- list(psub = list(cex = 0))
  params$traj <- list(plabels = list(cex = 0), psub = list(cex = 0))
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## prepare
  if(permute) {
    dfxy_row <- substitute(object$co)
    dfxy_col <- substitute(object$lisup)
    facets_row <- substitute(object$TC[,1])
    facets_col <- substitute(object$TL[,1])
  } else {
    dfxy_row <- substitute(object$lisup)
    dfxy_col <- substitute(object$co)
    facets_row <- substitute(object$TL[,1])
    facets_col <- substitute(object$TC[,1])
  }
  
  ## create g1
  g1 <- do.call("s.label", c(list(dfxy = dfxy_row, facets = facets_row, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))[which.tab]
  
  ## prepare and create g2
  if(permute)
    dcol <- object$lisup
  else
    dcol <- object$co
  k <- c(min(dcol[, xax]), max(dcol[, xax]), min(dcol[, yax]), max(dcol[, yax])) / c(g1[[1]]@g.args$xlim, g1[[1]]@g.args$ylim)
  dcol <- substitute(dfxy_col * 0.7 / max(k))
  g2 <- do.call("s.arrow", c(list(dfxy = dcol, facets = facets_col, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))[which.tab]
  obj <- do.call("superpose", list(g1, g2))
  obj@Call <- call("superpose", g1@Call, g2@Call)
  
  ## create g3
  if(traject) {
    g3 <- do.call("s.traject", c(list(dfxy = dfxy_row, facets = facets_row, xax = xax, yax = yax, plot = FALSE, storeData = FALSE, pos = pos - 2), sortparameters$traj))[which.tab]
    obj <- do.call("superpose", list(obj, g3))
    obj@Call <- call("superpose", obj@Call, g3@Call)
  }
	
  ## ADEgS creation
  names(obj) <- object$tab.names
  obj@Call <- match.call()
  if(plot) 
    print(obj)
  invisible(obj)
}


"kplot.pta" <- function(object, xax = 1, yax = 2, which.tab = 1:nrow(object$RV), which.graph = 1:4, pos = -1, storeData = FALSE, plot = TRUE, ...) {
  if(!inherits(object, "pta")) 
    stop("Object of class 'pta' expected")
  if((xax == yax) || (object$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > object$nf)
    stop("Non convenient xax")
  if(yax > object$nf)
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("axis", "rows", "columns", "components")
  sortparameters <- .paramsADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params$axis <- list(pbackground = list(box = FALSE), plabels = list(alpha = 1))
  params$rows <- list(plabels = list(alpha = 1))
  params$columns <- list()
  params$components <- list(pbackground = list(box = FALSE))
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  facets1 <- substitute(object$T4[, 1])
  g1 <- do.call("s.corcircle", c(list(dfxy = substitute(object$Tax), labels = substitute(object$T4[, 2]), facets = facets1, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$axis))[which.tab]
  names(g1) <- paste(graphsnames[1], "_", object$tab.names, sep = "")
  
  facets2 <- substitute(object$TL[, 1])
  g2 <- do.call("s.label", c(list(dfxy = substitute(object$Tli), labels = substitute(adegraphics:::extractlabels(rownames(object$Tli))), facets = facets2, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$rows))[which.tab]
  names(g2) <- paste(graphsnames[2], "_", object$tab.names, sep = "")
  
  facets3 <- substitute(object$TC[, 1])
  g3 <- do.call("s.arrow", c(list(dfxy = substitute(object$Tco), labels = substitute(adegraphics:::extractlabels(rownames(object$Tco))), facets = facets3, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$columns))[which.tab]
  names(g3) <- paste(graphsnames[3], "_", object$tab.names, sep = "")
  
  facets4 <- substitute(object$T4[, 1])
  g4 <- do.call("s.corcircle", c(list(dfxy = substitute(object$Tcomp), labels = substitute(object$T4[, 2]), facets = facets4, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$components))[which.tab]
  names(g4) <- paste(graphsnames[4], "_", object$tab.names, sep = "")
  
  ## ADEgS creation
  ng <- sum(sapply(c(g1, g2, g3, g4), function(x) length(x)))
  lay <- matrix(1:ng, ncol = 4)
  obj <- new(Class = "ADEgS", ADEglist = c(g1@ADEglist, g2@ADEglist, g3@ADEglist, g4@ADEglist), positions = layout2position(lay), add = matrix(0, ncol = ng, nrow = ng), Call = match.call())
  if(plot) 
    print(obj)
  invisible(obj)
}


"kplot.sepan" <- function(object, xax = 1, yax = 2, which.tab = 1:length(object$blo), permute = FALSE, traject = FALSE, posieig = "bottomleft", pos = -1, storeData = FALSE, plot = TRUE, ...) {
  if(!inherits(object, "sepan")) 
    stop("Object of class 'sepan' expected")
  if((xax == yax) || (length(object$Eig) == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > length(object$Eig))
    stop("Non convenient xax")
  if(yax > length(object$Eig))
    stop("Non convenient yax")
  
  ## prepare
  if(permute) {
    dfxy_row <- substitute(object$Co)
    dfxy_col <- substitute(object$Li)
    names_row <- substitute(adegraphics:::extractlabels(rownames(object$Co)))
    names_col <- substitute(adegraphics:::extractlabels(rownames(object$Li)))
    facets_row <- substitute(object$TC[,1])
    facets_col <- substitute(object$TL[,1])
  } else {
    dfxy_row <- substitute(object$Li)
    dfxy_col <- substitute(object$Co)
    names_row <- substitute(adegraphics:::extractlabels(rownames(object$Li)))
    names_col <- substitute(adegraphics:::extractlabels(rownames(object$Co)))
    facets_row <- substitute(object$TL[,1])
    facets_col <- substitute(object$TC[,1])
  }
  
  ## sort parameters for each graph
  graphsnames <- c("row", "col", "traj", "eig")
  sortparameters <- .paramsADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params$row <- list(psub = list(position = "bottomright"), samelimits = FALSE)
  params$traj <- list(psub = list(cex = 0, position = "bottomright"), plabels = list(cex = 0), samelimits = FALSE)
  params$col <- list(psub = list(cex = 0, position = "bottomright"), plabels = list(cex = 1.25))
  params$eig <- list(psub = list(text = ""), pbackground = list(box = TRUE), samelimits = FALSE)
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
	## create g1
  if(!traject) 
  	g1 <- do.call("s.label", c(list(dfxy = dfxy_row, labels = names_row, facets = facets_row, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))[which.tab]
  else 
    g1 <- do.call("s.traject", c(list(dfxy = dfxy_row, labels = names_row, facets = facets_row, xax = xax, yax = yax, plot = FALSE, storeData = FALSE, pos = pos - 2), sortparameters$traj))[which.tab]
  
  ## prepare and create g2
  if(permute)
    dcol <- object$Li
  else
    dcol <- object$Co
  k <- c(min(dcol[, xax]), max(dcol[, xax]), min(dcol[, yax]), max(dcol[, yax])) / c(g1[[1]]@g.args$xlim, g1[[1]]@g.args$ylim)
  dcol <- substitute(dfxy_col * 0.7 / max(k))
  g2 <- do.call("s.arrow", c(list(dfxy = dcol, labels = names_col, facets = facets_col, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))[which.tab]
  obj <- do.call("superpose", list(g1, g2))
  obj@Call <- call("superpose", g1@Call, g2@Call)
  
  ## prepare and create g3
  facets_eig <- reorder(as.factor(rep(levels(object$TL[, 1]), object$rank)), rep(1:length(object$rank), object$rank))
  if(!any(posieig == "none")) {
    g3 <- do.call(".add.scatter.eig", c(list(eigvalue = substitute(object$Eig), nf = 1:ncol(object$Li), facets = facets_eig, xax = xax, yax = yax, plot = FALSE), sortparameters$eig))[which.tab]
    obj <- do.call("insert", list(g3, obj, posi = posieig, plot = FALSE, ratio = 0.2, inset = 0, dispatch = TRUE))
  }
  
  ## ADEgS creation
  names(obj) <- object$tab.names
  obj@Call <- match.call()
  if(plot) 
    print(obj)
  invisible(obj)
} 


"kplot.sepan.coa" <- function(object, xax = 1, yax = 2, which.tab = 1:length(object$blo), permute = FALSE, posieig = "bottomleft", pos = -1, storeData = FALSE, plot = TRUE, ...) {
	if(!inherits(object, "sepan")) 
    stop("Object of class 'sepan' expected")
	if((xax == yax) || (length(object$Eig) == 1))
    stop("One axis only : not yet implemented")
	if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
	if(xax > length(object$Eig))
    stop("Non convenient xax")
	if(yax > length(object$Eig))
    stop("Non convenient yax")
  
  ## prepare
  if(permute) {
    dfxy_row <- substitute(object$C1)
    dfxy_col <- substitute(object$Li)
    names_row <- substitute(adegraphics:::extractlabels(rownames(object$C1)))
    names_col <- substitute(adegraphics:::extractlabels(rownames(object$Li)))
    facets_row <- substitute(object$TC[,1])
    facets_col <- substitute(object$TL[,1])
  } else {
    dfxy_row <- substitute(object$Li)
    dfxy_col <- substitute(object$C1)
    names_row <- substitute(adegraphics:::extractlabels(rownames(object$Li)))
    names_col <- substitute(adegraphics:::extractlabels(rownames(object$C1)))
    facets_row <- substitute(object$TL[,1])
    facets_col <- substitute(object$TC[,1])
  }
  
  ## sort parameters for each graph
  graphsnames <- c("row", "col", "eig")
  sortparameters <- .paramsADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params$col <- list(psub = list(position = "bottomright"), plabels = list(cex = 1.25), samelimits = FALSE)
  params$row <- list(psub = list(cex = 0, position = "bottomright"))
  params$eig <- list(psub = list(text = ""), pbackground = list(box = TRUE))
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation and create g1 and g2
	g1 <- do.call("s.label", c(list(dfxy = dfxy_col, labels = names_col, facets = facets_col, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))[which.tab]
  g2 <- do.call("s.label", c(list(dfxy = dfxy_row, labels = names_row, facets = facets_row, xax = 1, yax = 2, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))[which.tab]
  obj <- do.call("superpose", c(list(g1, g2)))
  obj@Call <- call("superpose", g1@Call, g2@Call)
  
  ## prepare and create g3
  facets_eig <- reorder(as.factor(rep(levels(object$TL[, 1]), object$rank)), rep(1:length(object$rank), object$rank))
  if(!any(posieig == "none")) {
    g3 <- do.call(".add.scatter.eig", c(list(eigvalue = substitute(object$Eig), nf = 1:ncol(object$Li), facets = facets_eig, xax = xax, yax = yax, plot = FALSE), sortparameters$eig))[which.tab]
    obj <- do.call("insert", list(g3, obj, posi = posieig, plot = FALSE, ratio = 0.2, inset = 0, dispatch = TRUE))
  }
  
  ## ADEgS creation
  names(obj) <- object$tab.names
  obj@Call <- match.call()
  if(plot) 
    print(obj)
  invisible(obj)
}


"kplot.statis" <- function(object, xax = 1, yax = 2, which.tab = 1:length(object$tab.names), traject = FALSE, arrow = TRUE, class = FALSE, pos = -1, storeData = FALSE, plot = TRUE, ...) {
  if(!inherits(object, "statis")) 
	  stop("Object of class 'statis' expected")
  if((xax == yax) || (object$C.nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > object$C.nf)
    stop("Non convenient xax")
  if(yax > object$C.nf)
    stop("Non convenient yax")
  
  ## sort parameters for each graph
  graphsnames <- c("col", "traj", "class")
  sortparameters <- .paramsADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params$col <- list()
  params$traj <- list(plabels = list(cex = 0), psub = list(cex = 0))
  params$class <- list(plabels = list(cex = 1.5), ppoints = list(cex = 2), pellipses = list(alpha = 0, axes = list(draw = FALSE)), psub = list(cex = 0))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## prepare
  facets <- substitute(object$TC[, 1])
  
  ## creation of each individual ADEg
  if(arrow) 
    g1 <- do.call("s.arrow", c(list(dfxy = substitute(object$C.Co), facets = facets, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))[which.tab]    	
  else
    g1 <- do.call("s.label", c(list(dfxy = substitute(object$C.Co), xax = xax, yax = yax, facets = facets, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))[which.tab]
  
  if(traject) {
    g2 <- do.call("s.traject", c(list(dfxy = substitute(object$C.Co), xax = xax, yax = yax, facets = facets, plot = FALSE, storeData = FALSE, pos = pos - 2), sortparameters$traj))[which.tab]
    obj <- do.call("superpose", list(g1, g2))
    obj@Call <- call("superpose", g1@Call, g2@Call)
  } else
    obj <- g1
  
  if(class) {
    g3 <- do.call("s.class", c(list(dfxy = substitute(object$C.Co), fac = object$TC[, 1], xax = xax, yax = yax, facets = facets, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$class))[which.tab]
    obj <- do.call("superpose", list(obj, g3))
    obj@Call <- call("superpose", obj@Call, g3@Call)
  }
	
  ## ADEgS creation
  names(obj) <- object$tab.names
  obj@Call <- match.call()
  if(plot) 
    print(obj)
  invisible(obj)
}
