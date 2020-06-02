s.Spatial <- function(spObj, col = TRUE, nclass = 5, scale = TRUE, plot = TRUE, storeData = TRUE, pos = -1, ...) {
    oldparamadeg <- adegpar()
    on.exit(adegpar(oldparamadeg))
    sortparameters <- sortparamADEg(...)
    adegtot <- adegpar(sortparameters$adepar)
    
    xy.spObj <- coordinates(spObj)[, , drop = FALSE]  ## to access 'coordinates' in the 'imports' environment of 'adegraphics'
    
    ## different cases (data or not, points or polygons)
    ## s.value is used for points with numeric data
    ## s.class is used for points with factor data
    ## s.label in other cases
    
    nvar <- 0
    if(length(grep("DataFrame", class(spObj))) > 0)
        nvar <- ncol(spObj)
    
    points.or.poly <- ifelse(length(grep("Poly", class(spObj))) > 0, "poly", "points")
    
    ## default values for parameters
    defaultpar <- list(pgrid = list(draw = FALSE), porigin = list(include = FALSE), 
        plegend = list(drawKey = ifelse(nvar == 1, TRUE, FALSE)), psub = list(position = "topleft"))
    
    sortparameters$adepar <- modifyList(defaultpar, sortparameters$adepar, keep.null = TRUE)
    
    ## limits management 
    limsSp <- bbox(spObj)
    lim.global <- setlimits2D(minX = limsSp[1, 1], maxX = limsSp[1, 2], minY = limsSp[2, 1], maxY = limsSp[2, 2], includeOr = FALSE) 
    if(is.null(sortparameters$g.args$xlim))
        sortparameters$g.args$xlim <- lim.global$xlim
    if(is.null(sortparameters$g.args$ylim))
        sortparameters$g.args$ylim <- lim.global$ylim
    
    if(nvar == 0){
        if(is.logical(col)){
            if(col)
                colnew <- adegtot$pSp$col
            else
                colnew <- "transparent"	## col == FALSE
        } else {
            colnew <- col
        }
        
        sortparameters$adepar$pSp$col <- colnew
        sortparameters$adepar$ppoint$cex <- 0
        ## create map
        if(points.or.poly == "points")
            object <- do.call("s.label", c(list(dfxy = xy.spObj, plot = FALSE, storeData = storeData, pos = pos - 2), 
                sortparameters$adepar, sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
        else
            object <- do.call("s.label", c(list(dfxy = xy.spObj, Sp = substitute(spObj), plot = FALSE, storeData = storeData, pos = pos - 2), 
                sortparameters$adepar, sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
        
    } else if(nvar > 0) {
        listGraph <- list()
        for(i in 1:nvar) {
            defaultpar <- list(psub = list(text = names(spObj)[i]), plabels = list(cex = 0))
            adepar.i <- modifyList(defaultpar, sortparameters$adepar, keep.null = TRUE)
            
            if(points.or.poly == "points" & is.numeric(spObj@data[, i])){
                ## points and numeric -> s.value
                if(is.logical(col))
                    colnew <- NULL ## default in s.value
                else colnew <- col
                listGraph <- c(listGraph, do.call("s.value", c(list(dfxy = xy.spObj, z = if(scale) scale (spObj@data[, i]) else spObj@data[, i], plot = FALSE, 
                    col = colnew, storeData = storeData, pos = pos - 2), adepar.i, sortparameters$trellis, sortparameters$g.args, sortparameters$rest)))
            } else if(points.or.poly == "points" & is.factor(spObj@data[, i])) {
                if(is.logical(col))
                    colnew <- adegtot$ppalette$quali(nlevels(as.factor(spObj@data[, i])))
                adepar.i <- modifyList(list(ppoints = list(cex = 2)), adepar.i , keep.null = TRUE)
                listGraph <- c(listGraph, do.call("s.class", c(list(dfxy = xy.spObj, starSize = 0, ellipseSize = 0, fac = spObj@data[, i], plot = FALSE, 
                    col = colnew, storeData = storeData, pos = pos - 2), adepar.i, sortparameters$trellis, sortparameters$g.args, sortparameters$rest)))
            } else {
                if(is.logical(col)) {
                    if(col) {
                        if(is.numeric(spObj@data[, i])) {
                            nclasspretty <- length(pretty(spObj@data[, i], nclass)) - 1
                            nclasspretty <- length(pretty(spObj@data[, i], nclasspretty)) - 1 ## repeated in order to have always the same number of class
                            colnew <- adegtot$ppalette$quanti(nclasspretty)
                        } else
                            colnew <- adegtot$ppalette$quali(nlevels(as.factor(spObj@data[, i])))[as.factor(spObj@data[, i])]
                    }
                } else {
                    colnew <- col
                }
                adepar.i$pSp$col <- colnew
                adepar.i$ppoint$cex <- 0
                
                ## create map
                listGraph <- c(listGraph, do.call("s.label", c(list(dfxy = xy.spObj, Sp = substitute(spObj[,i]), plot = FALSE, storeData = storeData, pos = pos - 2), adepar.i, sortparameters$trellis, sortparameters$g.args, sortparameters$rest)))
                
            } 
        }
        
        if(nvar == 1)
            object <- listGraph[[1]]
        else {
            names(listGraph) <- names(spObj)
            posmatrix <- layout2position(.n2mfrow(nvar), ng = nvar)
            object <- new(Class = "ADEgS", ADEglist = listGraph, positions = posmatrix, add = matrix(0, ncol = nvar, nrow = nvar), Call = match.call())
        }
    }
    
    
    if(plot)
        print(object)
    invisible(object) 
}

