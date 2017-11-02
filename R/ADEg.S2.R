setClass(
    Class = "ADEg.S2",
    contains = c("ADEg", "VIRTUAL"),
    slots = c(data = "list")
    )


setMethod(
    f = "initialize",
    signature = "ADEg.S2",
    definition = function(.Object, data = list(dfxy = NULL, xax = 1, yax = 2, frame = 0, storeData = TRUE), ...) {
        .Object <- callNextMethod(.Object, ...) ## ADEg initialize
        .Object@data <- data
        return(.Object)
    })


setMethod(
    f = "prepare",
    signature = "ADEg.S2",
    definition = function(object) {
        ## TODO: factorise les if
        name_obj <- deparse(substitute(object))
        if(object@data$storeData)
            dfxy <- object@data$dfxy
        else
            dfxy <- eval(object@data$dfxy, envir = sys.frame(object@data$frame))
        
        ## axes limits
        if(class(object) == "S2.corcircle") {
            object@trellis.par$panel.background$col <- "transparent"
            if(object@g.args$fullcircle) {
                if(is.null(object@g.args$xlim) || !identical(object@s.misc$xfullcircle.update, object@g.args$fullcircle)) {
                    minX <- -1
                    maxX <- 1
                } else {
                    minX <- object@g.args$xlim[1]
                    maxX <- object@g.args$xlim[2]
                }
                if(is.null(object@g.args$ylim) || !identical(object@s.misc$yfullcircle.update, object@g.args$fullcircle)) {
                    minY <- -1
                    maxY <- 1
                } else {
                    minY <- object@g.args$ylim[1]
                    maxY <- object@g.args$ylim[2]
                }
            } else {
                if(is.null(object@g.args$xlim) || !identical(object@s.misc$xfullcircle.update, object@g.args$fullcircle)) {
                    minX <- min(dfxy[, object@data$xax])
                    maxX <- max(dfxy[, object@data$xax])
                } else {
                    minX <- object@g.args$xlim[1]
                    maxX <- object@g.args$xlim[2]
                }
                if(is.null(object@g.args$ylim) || !identical(object@s.misc$yfullcircle.update, object@g.args$fullcircle)) {
                    minY <- min(dfxy[, object@data$yax])
                    maxY <- max(dfxy[, object@data$yax])
                } else {
                    minY <- object@g.args$ylim[1]
                    maxY <- object@g.args$ylim[2]
                }
            }
        } else {
            if(is.null(object@g.args$xlim)) {
                minX <- min(dfxy[, object@data$xax])
                maxX <- max(dfxy[, object@data$xax])
            } else {
                minX <- object@g.args$xlim[1]
                maxX <- object@g.args$xlim[2]
            }
            if(is.null(object@g.args$ylim)) {
                minY <- min(dfxy[, object@data$yax])
                maxY <- max(dfxy[, object@data$yax])
            } else {
                minY <- object@g.args$ylim[1]
                maxY <- object@g.args$ylim[2]
            }
        }
        
        limits <- setlimits2D(minX = minX, maxX = maxX, minY = minY, maxY = maxY, origin = rep(object@adeg.par$porigin$origin, le = 2),
                             aspect.ratio = object@adeg.par$paxes$aspectratio, includeOr = object@adeg.par$porigin$include)
        
        if(is.null(object@g.args$xlim) || !identical(object@s.misc$xfullcircle.update, object@g.args$fullcircle))
            object@g.args$xlim <- limits$xlim
        if(is.null(object@g.args$ylim) || !identical(object@s.misc$yfullcircle.update, object@g.args$fullcircle))
            object@g.args$ylim <- limits$ylim
        
        if(class(object) == "S2.corcircle") {
            object@s.misc$xfullcircle.update <- object@g.args$fullcircle
            object@s.misc$yfullcircle.update <- object@g.args$fullcircle
        }
        
        ## grid locations and axes 
        if(object@adeg.par$pgrid$draw || object@adeg.par$paxes$draw) {
            ## axes division
            if(class(object) != "S2.corcircle") {
                if(object@adeg.par$porigin$include)
                    object@s.misc$backgrid <- .getgrid(xlim = object@g.args$xlim, ylim = object@g.args$ylim, object@adeg.par$pgrid$nint, rep(object@adeg.par$porigin$origin, le = 2), asp = object@adeg.par$paxes$aspectratio)
                else
                    object@s.misc$backgrid <- .getgrid(xlim = object@g.args$xlim, ylim = object@g.args$ylim, object@adeg.par$pgrid$nint, asp = object@adeg.par$paxes$aspectratio)
            }
            
            if(object@adeg.par$paxes$draw) {
                ## parameters to plot axes
                scalesandlab <- list(x = object@adeg.par$paxes$x, y = object@adeg.par$paxes$y)
                if(is.null(scalesandlab$y$at)) {
                    scalesandlab$y$at <- object@s.misc$backgrid[[3L]][!is.na(object@s.misc$backgrid[[3L]])]
                    if(class(object) == "S2.corcircle")
                        scalesandlab$y$at <- scalesandlab$y$at[(length(scalesandlab$y$at) / 2 + 1):length(scalesandlab$y$at)]
                }
                if(is.null(scalesandlab$x$at)) {
                    scalesandlab$x$at <- object@s.misc$backgrid[[1L]][!is.na(object@s.misc$backgrid[[1L]])]
                    if(class(object) == "S2.corcircle")
                        scalesandlab$x$at <- scalesandlab$x$at[1:(length(scalesandlab$x$at) / 2)]
                }
            } else 
                scalesandlab <- list(draw = FALSE) ## no axes
        }
        else
            scalesandlab <- list(draw = FALSE) ## no axes
        
        if(object@adeg.par$paxes$aspectratio != "iso")
            object@adeg.par$pgrid$text$cex <- 0 ## grid cell size has no meaning
        
        if(!is.null(object@g.args$Sp))
          object@adeg.par$paxes$aspectratio <- ifelse(is.na(proj4string(object@g.args$Sp)) || is.projected(object@g.args$Sp), 1, 1/cos((mean(object@g.args$ylim) * pi)/180))
        
        ## if grid and axes are drawn, no text indication
        if(object@adeg.par$pgrid$draw && object@adeg.par$paxes$draw)
            object@adeg.par$pgrid$text$cex <- 0
        object@g.args$scales <- scalesandlab
        assign(name_obj, object, envir = parent.frame())
    })


setMethod(
    f = "panelbase",
    signature = "ADEg.S2",
    definition = function(object, x, y) {
        ## draw grid
        lims <- current.panel.limits(unit = "native")
        porigin <- object@adeg.par$porigin
        porigin$origin <- rep(porigin$origin, length.out = 2)

        if(class(object) == "S2.corcircle") 
            grid.circle(x = 0, y = 0, r = 1, default.units = "native", gp = gpar(col = "black", fill = object@adeg.par$pbackground$col), draw = TRUE, name = "circleGrid")
        
        if(object@adeg.par$pgrid$draw) { ## if grid to draw
            grid <- object@adeg.par$pgrid
            locations <- object@s.misc$backgrid ## coordinates for the grid 
            panel.segments(
                x0 = c(locations$x0[!is.na(locations$x0)], rep(lims$xlim[1], sum(is.na(locations$x0)))),
                x1 = c(locations$x1[!is.na(locations$x1)], rep(lims$xlim[2], sum(is.na(locations$x1)))),
                y0 = c(rep(lims$ylim[1], sum(is.na(locations$y0))), locations$y0[!is.na(locations$y0)]),
                y1 = c(rep(lims$ylim[2], sum(is.na(locations$y1))), locations$y1[!is.na(locations$y1)]),
                col = grid$col, lty = grid$lty, lwd = grid$lwd)
            
            if(grid$text$cex > 0) {
                text.pos <- .setposition(grid$text$pos)
                textgrid <- textGrob(label = paste("d =", locations$d), x = text.pos$posi[1], y = text.pos$posi[2], just = text.pos$just, gp = gpar(cex = grid$text$cex, col = grid$text$col), name = "gridtext")
                grid.rect(x = text.pos$posi[1], y = text.pos$posi[2], width = grobWidth(textgrid), height = grobHeight(textgrid),
                          just = text.pos$just, gp = gpar(fill = ifelse(class(object) == "S2.corcircle", "transparent", object@adeg.par$pbackground$col), alpha = 1, col = "transparent"))
                grid.draw(textgrid)
            }
        }
        
        if(porigin$draw && porigin$include & class(object) == "S2.corcircle") {
            panel.segments(x0 = c(-1, porigin$origin[1]), x1 = c(1, porigin$origin[1]), y0 = c(porigin$origin[2], -1), y1 = c(porigin$origin[2], 1), col = porigin$col, lwd = porigin$lwd, lty = porigin$lty, alpha = porigin$alpha)
            ## TODO: check last parameters valididy     
        }
        
        if(porigin$draw && porigin$include & !class(object) == "S2.corcircle") {
            panel.abline(h = porigin$origin[2], v = porigin$origin[1], col = porigin$col, lwd = porigin$lwd, lty = porigin$lty, alpha = porigin$alpha)
            ## TODO: check last parameters valididy
        }
        
        ## spatial object management
        if(any(names(object@g.args) == "Sp")) {
            do.call("adeg.panel.Spatial", args = c(list(SpObject = object@g.args$Sp, sp.layout = object@g.args$sp.layout), object@adeg.par$pSp))
        }
        else  ## no Sp but sp.layout
            if(any(names(object@g.args) == "sp.layout"))
              sppanel(lst = object@g.args$sp.layout)
        
        ## neighbouring object management
        if(any(names(object@g.args) == "nbobject")) {
            nbobj <- object@g.args$nbobject
            if(!inherits(nbobj, "nb") & !inherits(nbobj, "listw"))
                stop("wrong class for the nb object")
            pnb <- object@adeg.par$pnb
            do.call("adeg.panel.nb", args = list(nbobject = nbobj, coords = cbind(x, y), col.edge = pnb$edge$col, lwd = pnb$edge$lwd, lty = pnb$edge$lty, pch = pnb$node$pch, cex = pnb$node$cex, col.node = pnb$node$col, alpha = pnb$node$alpha))
        }
        callNextMethod()
    })


setMethod(
    f = "setlatticecall",
    signature = "ADEg.S2",
    definition =  function(object) {
        ## arguments recurrents de la liste, pas les limites car elles seront definis ensuite
        name_obj <- deparse(substitute(object))

        ## background and box
        if(!inherits(object, "S2.corcircle"))
            object@trellis.par$panel.background$col <- object@adeg.par$pbackground$col
        if(!object@adeg.par$pbackground$box)
          object@trellis.par$axis.line$col <- "transparent"
        else
          object@trellis.par$axis.line$col <- "black"
        
        arguments <- list(
            par.settings = object@trellis.par,
            scales = object@g.args$scales,
            aspect = object@adeg.par$paxes$aspectratio,
            key = createkey(object),
            legend = createcolorkey(object),
            axis = axis.L, ## see utils.R
            panel = function(...) {
                panelbase(object,...) ## grid,
                panel(object,...) ## call to S2.panel function, for slabel and ADEg.S2 class of graphs
            })

        object@lattice.call$arguments <- arguments          
        object@lattice.call$graphictype <- "xyplot"

        ## get lattice arguments (set unspecified to NULL)
        argnames <- c("main", "sub", "xlab", "ylab")
        largs <- object@g.args[argnames]
        names(largs) <- argnames
        ## add xlim and ylim if not NULL
        if("xlim" %in% names(object@g.args))
            largs["xlim"] <- object@g.args["xlim"]
        if("ylim" %in% names(object@g.args))
            largs["ylim"] <- object@g.args["ylim"]
        
        object@lattice.call$arguments <- c(object@lattice.call$arguments, largs, list(strip = FALSE))
        assign(name_obj, object, envir = parent.frame())
    })


## zoom without center
setMethod(
    f = "zoom",
    signature = c("ADEg.S2", "numeric", "missing"),
    definition  =  function(object, zoom, center) {
        oldxlim <- object@g.args$xlim
        oldylim <- object@g.args$ylim
        if(length(zoom) != 1)
            stop("zoom factor should be length 1")
        diffx <- diff(oldxlim)
        diffy <- diff(oldylim)
        center <- c(oldxlim[1] + diffx / 2, oldylim[1] + diffy / 2)
        diffx <- diffx / zoom
        diffy <- diffy / zoom
        object@g.args$xlim <- c(center[1] - diffx / 2, center[1] + diffx / 2)
        object@g.args$ylim <- c(center[2] - diffy / 2, center[2] + diffy / 2)
        if(object@adeg.par$pgrid$draw || object@adeg.par$paxes$draw)
            object@s.misc$backgrid <- .getgrid(xlim = object@g.args$xlim, ylim = object@g.args$ylim, object@adeg.par$pgrid$nint, object@adeg.par$porigin$origin, asp = object@adeg.par$paxes$aspectratio)
        prepare(object)
        setlatticecall(object)
        print(object)
        invisible()
    })


## zoom with center
setMethod(
    f = "zoom",
    signature = c("ADEg.S2", "numeric", "numeric"),
    definition = function(object, zoom, center) {
        if(length(center) != 2) 
            stop("error, center should be length 2")
        if(length(zoom) != 1) 
            stop("zoom factor should be length 1")
        diffx <- diff(object@g.args$xlim) / zoom
        diffy <- diff(object@g.args$ylim) / zoom
        object@g.args$xlim <- c(center[1] - diffx / 2, center[1] + diffx / 2)
        object@g.args$ylim <- c(center[2] - diffy / 2, center[2] + diffy / 2)
        if(object@adeg.par$pgrid$draw || object@adeg.par$paxes$draw)
            object@s.misc$backgrid <- .getgrid(xlim = object@g.args$xlim, ylim = object@g.args$ylim, object@adeg.par$pgrid$nint, object@adeg.par$porigin$origin, asp = object@adeg.par$paxes$aspectratio)
        prepare(object)
        setlatticecall(object)
        print(object)
        invisible()
    })


setMethod(
    f = "gettrellis",
    signature = "ADEg.S2",
    definition = function(object) {
        if(object@data$storeData) {
            dfxy <- as.matrix(object@data$dfxy)
            xax <- object@data$xax
            yax <- object@data$yax
        } else {
            dfxy <- as.matrix(eval(object@data$dfxy, envir = sys.frame(object@data$frame)))
            yax <- eval(object@data$yax, envir = sys.frame(object@data$frame))
            xax <- eval(object@data$xax, envir = sys.frame(object@data$frame))
        }
        
        tmptrellis <- do.call(what = object@lattice.call$graphictype, args = c(formula(dfxy[, yax] ~ dfxy[, xax]), object@lattice.call$arguments, environment()))
        return(tmptrellis)
    })
