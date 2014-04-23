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
      colnew <- "transparent"	## col == FALSE
  
  nvar <- 0
  if(length(grep("DataFrame", class(spObj))) > 0)
    nvar <- ncol(spObj)

  if(nvar < 2) {
    if(nvar == 1) {
      ## Spatial*DataFrame object -> ADEg
      sortparameters$adepar$psub$text <- modifyList(names(spObj)[1], sortparameters$adepar$psub$text, keep.null = TRUE)
      if(is.logical(col)) {
        if(col) {
          if(is.numeric(spObj@data[, 1])) {
            nclasspretty <- length(pretty(spObj@data[, 1], nclass)) - 1
            nclasspretty <- length(pretty(spObj@data[, 1], nclasspretty)) - 1 ## repeated in order to have always the same number of class
            colnew <- adegtot$ppalette$quanti(nclasspretty)
          } else
            colnew <- adegtot$ppalette$quali(nlevels(as.factor(spObj@data[, 1])))
        }
      } else {
        colnew <- col
      }
    } else {
      ## Spatial object (no data)
      colnew <- adegtot$pSp$col
    }

    sortparameters$adepar$pSp$col <- colnew
    ## create map
    object <- do.call("s.label", c(list(dfxy = substitute(sp::coordinates(spObj)), Sp = substitute(spObj), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args))
    
  } else {
    ## Spatial*DataFrame object with several variables -> ADEgS
    listGraph <- list()
    defaultpar <- list(psub = list(text = names(spObj)))
    sortparameters$adepar <- modifyList(defaultpar, sortparameters$adepar, keep.null = TRUE)
    names <- sortparameters$adepar$psub$text
    for(i in 1:nvar) {
      if(is.logical(col)) {
        if(col) {
          if(is.numeric(spObj@data[, i])) {
            nclasspretty <- length(pretty(spObj@data[, i], nclass)) - 1
            nclasspretty <- length(pretty(spObj@data[, i], nclasspretty)) - 1 ## repeated in order to have always the same number of class
            colnew <- adegtot$ppalette$quanti(nclasspretty)
          } else
            colnew <- adegtot$ppalette$quali(nlevels(as.factor(spObj@data[, i])))
        }
      } else {
        colnew <- col
      }
      
      sortparameters$adepar$pSp$col <- colnew
      sortparameters$adepar$psub$text <- names[i] 
      ## create map
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

