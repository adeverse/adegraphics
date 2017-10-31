"scatter.dudi" <- function(x, xax = 1, yax = 2, permute = FALSE, posieig = "topleft", prop = FALSE, 
  density.plot = ifelse(permute, ncol(x$tab) > 1000, nrow(x$tab) > 1000), plot = TRUE, storeData = TRUE, pos = -1, ...) {
  if(!inherits(x, "dudi")) 
    stop("Object of class 'dudi' expected")
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")  
  
  position <- match.arg(posieig[1], choices = c("bottomleft", "bottomright", "topleft", "topright", "none"), several.ok = FALSE)
  
  ## sort parameters for each graph
  graphsnames <- c("row", "col", "eig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params$row <- list(plabels = list(cex = 0.75))
  params$col <- list()
  params$eig <- list(pbackground = list(box = TRUE), psub = list(text = "Eigenvalues"))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  if(prop) {
    id <- inertia.dudi(x, col.inertia = TRUE)
    if(is.null(sortparameters[[2]]$plabels$cex)) {
      sortparameters$col$plabels$cex <- id$col.cum[, 2] / (max(id$col.cum[, 2]) / 1.5)
    } else {
	    sortparameters$col$plabels$cex <- sortparameters$col$plabels$cex * id$col.cum[, 2] / (max(id$col.cum[, 2]) / 1.5)
    }
  }
  
  ## prepare and create g1
  if(permute)
    df1 <- substitute(x$co)
  else
    df1 <- substitute(x$li)
  g1 <- do.call(ifelse(density.plot, "s.density", "s.label"), c(list(dfxy = df1, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))
  
  ## prepare and create g2
  if(permute) {
    colss <- x$l1
  } else {
    colss <- x$c1
  }
  knormali <- c(min(colss[, xax]), max(colss[, xax]), min(colss[, yax]), max(colss[, yax])) / c(g1@g.args$xlim, g1@g.args$ylim)
  csts <- 0.9 / max(knormali)
  if(permute) {
    df2 <- substitute(x$l1 * csts)
  } else {
    df2 <- substitute(x$c1 * csts)
  }
  g2 <- do.call("s.arrow", c(list(dfxy = df2, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))
  
  ## create the final ADEgS
  object <- do.call("superpose", list(g1, g2))
  object@Call <- call("superpose", g1@Call, g2@Call)
  if(position != "none") {
    g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$eig))
    object <- do.call("insert", list(g3@Call, object@Call, posi = position, plot = FALSE, ratio = 0.25))
  }
  
  names(object) <- graphsnames[1:length(object)]
  object@Call <- match.call()
  if(plot) 
    print(object)
  invisible(object)
}


"scatter.coa" <- function(x, xax = 1, yax = 2, method = 1:3, posieig = "topleft", pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "dudi"))
    stop("Object of class 'dudi' expected")
  if(!inherits(x, "coa"))
    stop("Object of class 'coa' expected")
  
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  position <- match.arg(posieig[1], choices = c("bottomleft", "bottomright", "topleft", "topright", "none"), several.ok = FALSE)
  method <- method[1]
  
  ## limits management
  if(method == 1)
    x.global <- rbind(as.matrix(x$li), as.matrix(x$co))
  else if(method == 2)
    x.global <- rbind(as.matrix(x$c1), as.matrix(x$li))
  else if(method == 3)
    x.global <- rbind(as.matrix(x$l1), as.matrix(x$co))
  adegtot <- adegpar()
  lim.global <- setlimits2D(minX = min(x.global[, xax]), maxX = max(x.global[, xax]), minY = min(x.global[, yax]), maxY = max(x.global[, yax]), origin = adegtot$porigin$origin, aspect.ratio = adegtot$paxes$aspectratio, includeOr = adegtot$porigin$include)
  
  ## sort parameters for each graph
  graphsnames <- c("row", "col", "eig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params$row <- list(plabels = list(cex = 0.75), xlim = lim.global$xlim, ylim = lim.global$ylim)
  params$col <- list(xlim = lim.global$xlim, ylim = lim.global$ylim)
  params$eig <- list(pbackground = list(box = TRUE), psub = list(text = "Eigenvalues"))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg and of the final ADEgS
  if(method == 1) {
    g1 <- do.call("s.label", c(list(dfxy = substitute(x$li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))
    g2 <- do.call("s.label", c(list(dfxy = substitute(x$co), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))
  } else if(method == 2) {
    g1 <- do.call("s.label", c(list(dfxy = substitute(x$c1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))
    g2 <- do.call("s.label", c(list(dfxy = substitute(x$li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))
  } else if(method == 3) {
    g1 <- do.call("s.label", c(list(dfxy = substitute(x$l1), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))
    g2 <- do.call("s.label", c(list(dfxy = substitute(x$co), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))
  }  
  object <- do.call("superpose", list(g1, g2))
  object@Call <- call("superpose", g1@Call, g2@Call)
  if(position != "none") {
    g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$eig))
    object <- do.call("insert", list(g3@Call, object@Call, posi = position, plot = FALSE, ratio = 0.25))
  }
  
  object@Call <- match.call()
  names(object) <- graphsnames[1:length(object)]
  if(plot) 
    print(object)
  invisible(object)
}


"scatter.pco" <- function(x, xax = 1, yax = 2, posieig = "topleft", pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "dudi"))
    stop("Object of class 'dudi' expected")
  if(!inherits(x, "pco"))
    stop("Object of class 'pco' expected")
  
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  position <- match.arg(posieig[1], choices = c("bottomleft", "bottomright", "topleft", "topright", "none"), several.ok = FALSE)
  
  ## sort parameters for each graph
  graphsnames <- c("row", "eig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params$row <- list()
  params$eig <- list(pbackground = list(box = TRUE), psub = list(text = "Eigenvalues"))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg and of the final ADEgS
  object <- do.call("s.label", c(list(dfxy = substitute(x$li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))
  if(position != "none") {
    g2 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$eig))
    object <- do.call("insert", list(g2@Call, object@Call, posi = position, plot = FALSE, ratio = 0.25))
    names(object) <- graphsnames[1:length(object)]
  }
  
  object@Call <- match.call()
  if(plot)
    print(object)
  invisible(object)
}


"scatter.nipals" <- function(x, xax = 1, yax = 2, posieig = "topleft", pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "nipals"))
    stop("Object of class 'nipals' expected")
  
  if((xax == yax) || (x$nf == 1))
    stop("One axis only : not yet implemented")
  if(length(xax) > 1 | length(yax) > 1)
    stop("Not implemented for multiple xax/yax")
  
  if(xax > x$nf)
    stop("Non convenient xax")
  if(yax > x$nf)
    stop("Non convenient yax")
  
  position <- match.arg(posieig[1], choices = c("bottomleft", "bottomright", "topleft", "topright", "none"), several.ok = FALSE)
  
  ## sort parameters for each graph
  graphsnames <- c("row", "col", "eig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  params <- list()
  params$row <- list(plabels = list(cex = 0.75))
  params$col <- list()
  params$eig <- list(pbackground = list(box = TRUE), psub = list(text = "Eigenvalues"))
  names(params) <- graphsnames
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## prepare and create g1
  g1 <- do.call("s.label", c(list(dfxy = substitute(x$li), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$row))
  
  ## prepare and create g2
  knormali <- c(min(x$c1[, xax]), max(x$c1[, xax]), min(x$c1[, yax]), max(x$c1[, yax])) / c(g1@g.args$xlim, g1@g.args$ylim)
  csts <- 0.8 / max(knormali)
  df2 <- substitute(x$c1 * csts)
  g2 <- do.call("s.arrow", c(list(dfxy = df2, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$col))
  
  ## creation of each individual ADEg and of the final ADEgS
  object <- do.call("superpose", list(g1, g2))
  object@Call <- call("superpose", g1@Call, g2@Call)
  if(position != "none") {
    g3 <- do.call("plotEig", c(list(eigvalue = substitute(x$eig), nf = 1:x$nf, xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$eig))
    object <- do.call("insert", list(g3@Call, object@Call, posi = position, plot = FALSE, ratio = 0.25))
  }
  
  names(object) <- graphsnames[1:length(object)]
  object@Call <- match.call()
  if(plot)
    print(object)
  invisible(object)
}

