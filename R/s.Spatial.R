s.Spatial <- function(spObj, col = TRUE, nclass = 5, plot = TRUE, storeData = FALSE, pos = -1, ...) {
  oldparamadeg <- adegpar()
  on.exit(adegpar(oldparamadeg))
  sortparameters <- .specificpar(...)
  adegtot <- adegpar(sortparameters$adepar)
  
  ## default values for non-used parameters
  defaultpar <- list(plabels = list(cex = 0), pgrid = list(draw = FALSE), ppoints = list(cex = 0), porigin = list(include = FALSE))
  sortparameters$adepar <- modifyList(defaultpar, sortparameters$adepar, keep.null = TRUE)
  if(is.logical(col))
    if(!col)
      col <- "transparent"	## col == FALSE
  
  nvar <- 0
  if(length(grep("DataFrame", class(spObj))) > 0)
    nvar <- ncol(spObj)
  
  if(nvar < 2) {
    if(nvar == 1) {
      ## Spatial*DataFrame object -> ADEg
      sortparameters$adepar$psub$text <- names(spObj)[1]
      if(is.logical(col))
        if(col) {
          if(is.numeric(spObj@data[, 1]))
            col <- adegtot$ppalette$quanti(nclass)
          else
            col <- adegtot$ppalette$quali(nlevels(as.factor(spObj@data[, 1])))
        }
    } else {
      ## Spatial object (no data)
      col <- adegtot$pSp$col
    }
    
    sortparameters$adepar$pSp$col <- col
    ## create map 
    object <- do.call("s.label", c(list(dfxy = substitute(sp::coordinates(spObj)), Sp = substitute(spObj), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args))
  } else {
    ## Spatial*DataFrame object with several variables -> ADEgS
    listGraph <- list()
    for(i in 1:nvar) {
      if(is.logical(col))
        if(col) {
          if(is.numeric(spObj@data[, i]))
            col <- adegtot$ppalette$quanti(nclass)
          else
            col <- adegtot$ppalette$quali(nlevels(as.factor(spObj@data[, i])))
        }
      sortparameters$adepar$pSp$col <- col
      sortparameters$adepar$psub$text <- names(spObj)[i]
      
      listGraph <- c(listGraph, do.call("s.label", c(list(dfxy = substitute(sp::coordinates(spObj)), Sp = substitute(spObj[, i]), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args)))
    }
    names(listGraph) <- names(spObj)
    posmatrix <- layout2position(n2mfrow(nvar), ng = nvar)
    object <- new(Class = "ADEgS", ADEglist = listGraph, positions = posmatrix, add = matrix(0, ncol = nvar, nrow = nvar), Call = match.call())
  }
  
  if(plot)
    print(object)
  invisible(object) 
}

