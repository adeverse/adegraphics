setMethod(
  f = "addhist",
  signature = "ADEg.S2",
  definition = function(object, bandwidth, gridsize = 60, kernel = "normal", cbreaks = 2, storeData = TRUE, plot = TRUE, pos = -1, ...) {
    thecall <- .expand.call(match.call())
    dfcall <- thecall$object
    dfxycall <- substitute(dfcall@data$dfxy)
    
    if(!(inherits(object, "ADEg.S2")))
      stop("Only implemented for 'ADEg.S2' object")
    
    if(storeData) {
      dfxy <- object@data$dfxy
      xax <- object@data$xax
      yax <- object@data$yax
    } else {
      dfxy <- eval(object@data$dfxy, envir = sys.frame(object@data$frame))
      xax <- eval(object@data$xax, envir = sys.frame(object@data$frame))
      yax <- eval(object@data$yax, envir = sys.frame(object@data$frame))
    }
    
    ## sorting parameters
    graphsnames <- c(all.names(substitute(object)), "densX", "densY", "link") 
    sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
    params <- vector("list", 4)
    names(params) <- graphsnames
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    update(object, sortparameters[[1]], plot = FALSE)
    
    ## setting positions
    positions <- layout2position(matrix(c(2, 4, 1, 3), 2, 2, byrow = TRUE), c(3, 1) / 2, c(3, 1) / 2, FALSE)
    
    ## grid computation
    xlimX <- object@g.args$xlim
    ylimY <- object@g.args$ylim
    breaks <- object@s.misc$backgrid
    cgrid <- breaks$d / cbreaks
    bb1 <- range(breaks$x0[!is.na(breaks$x0)])
    bb2 <- range(breaks$y0[!is.na(breaks$y0)])
    breaksX <- seq(from = bb1[1], to = bb1[2], by = cgrid)
    breaksY <- seq(from = bb2[1], to = bb2[2], by = cgrid)
    while(min(breaksX) > xlimX[1])
      breaksX <- c((min(breaksX) - cgrid), breaksX)
    while(max(breaksX) < xlimX[2])
      breaksX <- c(breaksX, (max(breaksX) + cgrid))
    while(min(breaksY) > ylimY[1])
      breaksY <- c((min(breaksY) - cgrid), breaksY)
    while(max(breaksY) < ylimY[2])
      breaksY <- c(breaksY, (max(breaksY) + cgrid))
    
    ## limits and graduation
    dfxaxcall <- call("[", dfxycall, 1:NROW(eval(dfxycall)), substitute(xax))
    dfxaxcallplus <- call("~", dfxaxcall, 1)
    dfyaxcall <- call("[", dfxycall, 1:NROW(eval(dfxycall)), substitute(yax))
    dfyaxcallplus <- call("~", dfyaxcall, 1)
    limcalX <- hist(dfxy[, xax], breaksX, plot = FALSE)
    limcalXcall <- call("hist", substitute(dfxaxcall), breaksX, plot = FALSE)
    limcalY <- hist(dfxy[, yax], breaksY, plot = FALSE)
    limcalYcall <- call("hist", substitute(dfyaxcall), breaksY, plot = FALSE)
    
    top <- 1.1 * max(c(limcalX$counts, limcalY$counts))
    xlimY <- ylimX <- c(0, top)
    drawLines <- pretty(0:top)
    drawLines <- drawLines[-c(1, length(drawLines))]
    
    if(!missing(bandwidth)) {
      densiX <- bkde(dfxy[, xax], kernel = kernel, bandwidth = bandwidth, gridsize = gridsize)
      densiXcall <- call("bkde", substitute(dfxaxcall), kernel = kernel, bandwidth = bandwidth, gridsize = gridsize)
      densiY <- bkde(dfxy[, yax], kernel = kernel, bandwidth = bandwidth, gridsize = gridsize)
      densiYcall <- call("bkde", substitute(dfyaxcall), kernel = kernel, bandwidth = bandwidth, gridsize = gridsize)
    } else {
      densiX <- bkde(dfxy[, xax], kernel = kernel, gridsize = gridsize)
      densiXcall <- call("bkde", substitute(dfxaxcall), kernel = kernel, gridsize = gridsize)
      densiY <- bkde(dfxy[, yax], kernel = kernel, gridsize = gridsize)
      densiYcall <- call("bkde", substitute(dfyaxcall), kernel = kernel, gridsize = gridsize)
    }
    
    ## trellis creation 
    g2 <- xyplot(dfxy[, xax] ~ 1, xlim = xlimX, ylim = ylimX, horizontal = TRUE, scales = list(draw = FALSE), xlab = NULL, ylab = NULL, histValues = limcalX, 
                 drawLines = drawLines, densi = densiX, params = sortparameters[[2]], 
                 panel = function(histValues, horizontal, drawLines, densi, params) adeg.panel.hist(histValues = histValues, horizontal = horizontal, 
                                                                                                    drawLines = drawLines, densi = densi, params = params))
    g2$call <- call("xyplot", dfxaxcallplus, xlim = substitute(xlimX), ylim = substitute(ylimX), horizontal = TRUE, scales = list(draw = FALSE), xlab = NULL, ylab = NULL, 
                    histValues = limcalXcall, drawLines = substitute(drawLines), densi = substitute(densiXcall), params = sortparameters[[2]], 
                    panel = function(histValues, horizontal, drawLines, densi, params) adeg.panel.hist(histValues = histValues, horizontal = horizontal, 
                                                                                                       drawLines = drawLines, densi = densi, params = params))
    
    
    g3 <- xyplot(dfxy[, yax] ~ 1, xlim = xlimY, ylim = ylimY, horizontal = FALSE, scales = list(draw = FALSE), xlab = NULL, ylab = NULL, histValues = limcalY, 
                 drawLines = drawLines, densi = densiY, params = sortparameters[[3]], 
                 panel = function(histValues, horizontal, drawLines, densi, params) adeg.panel.hist(histValues = histValues, horizontal = horizontal, 
                                                                                                    drawLines = drawLines, densi = densi, params = params))
    g3$call <- call("xyplot", dfyaxcallplus, xlim = substitute(xlimY), ylim = substitute(ylimY), horizontal = FALSE, scales = list(draw = FALSE), xlab = NULL, ylab = NULL, 
                    histValues = limcalYcall, drawLines = substitute(drawLines), densi = substitute(densiYcall), params = sortparameters[[3]], 
                    panel = function(histValues, horizontal, drawLines, densi, params) adeg.panel.hist(histValues = histValues, horizontal = horizontal, 
                                                                                                       drawLines = drawLines, densi = densi, params = params))
    
    
    g4 <- xyplot(1 ~ 1, xlim = xlimY, ylim = ylimX, scales = list(draw = FALSE), xlab = NULL, ylab = NULL, drawLines = drawLines, params = sortparameters[[4]], 
                 panel = function(drawLines, params) adeg.panel.join(drawLines = drawLines, params = params))
    g4$call <- call("xyplot", 1 ~ 1, xlim = substitute(xlimY), ylim = substitute(ylimX), scales = list(draw = FALSE), xlab = NULL, ylab = NULL, drawLines = substitute(drawLines), 
                    params = sortparameters[[4]], panel = function(drawLines, params) adeg.panel.join(drawLines = drawLines, params = params))
    
    ## ADEgS creation and display
    obj <- new(Class = "ADEgS", ADEglist = list(object, g2, g3, g4), positions = positions, add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
    names(obj) <- graphsnames
    if(plot)
      print(obj)
    invisible(obj)
  })