####################################################
##              Table/matrix/dist plot            ##
####################################################

setClass(
  Class = "ADEg.T",
  contains = c("ADEg", "VIRTUAL"),
  slots = c(data = "list")
  )


setMethod(
  f = "initialize",
  signature  = "ADEg.T",
  definition = function(.Object, data = list(dftab = NULL, coordsx = NULL, coordsy = NULL, labelsx = NULL, labelsy = NULL, frame = 0, storeData = TRUE), ...) {
    ## import the data in @data if storeData = TRUE
    .Object <- callNextMethod(.Object, ...) ## ADEg initialize
    if(data$storeData){
      data$dftab <- eval(data$dftab, envir = sys.frame(data$frame))
      data$coordsx <- eval(data$coordsx, envir = sys.frame(data$frame))
      data$coordsy <- eval(data$coordsy, envir = sys.frame(data$frame))
      data$labelsx <- eval(data$labelsx, envir = sys.frame(data$frame))
      data$labelsy <- eval(data$labelsy, envir = sys.frame(data$frame))
    }
    .Object@data <- data
    return(.Object)
  })


setMethod(
  f = "prepare",
  signature = "ADEg.T",
  definition = function(object) {
    name_obj <- deparse(substitute(object))
    if(object@data$storeData){
      coordsx <- object@data$coordsx
      coordsy <- object@data$coordsy
    } else {
      coordsx <- eval(object@data$coordsx, envir = sys.frame(object@data$frame))
      coordsy <- eval(object@data$coordsy, envir = sys.frame(object@data$frame))
    }

    ## cell size
    object@s.misc$axes$dx <- ifelse(length(coordsx) == 1, 1, diff(range(coordsx)) / length(coordsx))
    object@s.misc$axes$dy <- ifelse(length(coordsy) == 1, 1, diff(range(coordsy)) / length(coordsy))
    
    ## ll, rr, tt, bb: right, left, top, bottom margins
    ll <- rr <- rep(object@adeg.par$ptable$y$cstmargin, length.out = 2)[2]
    tt <- bb <- rep(object@adeg.par$ptable$x$cstmargin, length.out = 2)[2]
    if(object@adeg.par$ptable$y$pos == "left")
      ll <- object@adeg.par$ptable$y$cstmargin[1]
    else if(object@adeg.par$ptable$y$pos == "right")
      rr <- object@adeg.par$ptable$y$cstmargin[1] 
    object@g.args$xlim <- range(coordsx) + c(-1, 1) * object@s.misc$axes$dx
    if(object@adeg.par$ptable$x$pos == "top")
      tt <- object@adeg.par$ptable$x$cstmargin[1]
    else if(object@adeg.par$ptable$x$pos == "bottom")
      bb <- object@adeg.par$ptable$x$cstmargin[1]
    object@g.args$ylim <- range(coordsy) + c(-1, 1) * object@s.misc$axes$dy
    
    object@trellis.par <- c(object@trellis.par, list(clip = list(panel = "off"),
                                                     layout.heights = list(top.padding = tt, bottom.padding = bb),
                                                     layout.widths = list(left.padding = ll, right.padding = rr)))
    assign(name_obj, object, envir = parent.frame())
  })


setMethod(
  f = "panelbase",
  signature = "ADEg.T",
  definition = function(object, x, y) {
    callNextMethod()
    
    ## draw the box and the segments
    grid <- object@adeg.par$pgrid
    ## draw grid
    if(object@data$storeData) {
      xpos <- object@data$coordsx
      ypos <- object@data$coordsy
      labelsx <- object@data$labelsx
      labelsy <- object@data$labelsy
    } else {
      xpos <- eval(object@data$coordsx, envir = sys.frame(object@data$frame))
      ypos <- eval(object@data$coordsy, envir = sys.frame(object@data$frame))
      labelsx <- eval(object@data$labelsx, envir = sys.frame(object@data$frame))
      labelsy <- eval(object@data$labelsy, envir = sys.frame(object@data$frame))
    }
  
    nx <- length(xpos)
    ny <- length(ypos)

    ## draw grid (except for T.image)
    if(grid$draw & !inherits(object, "T.image"))
      panel.segments(
        x0 = c(xpos, rep(min(xpos)- object@s.misc$axes$dx, length.out = ny)),
        x1 = c(xpos, rep(max(xpos) + object@s.misc$axes$dx, length.out = ny)),
        y0 = c(rep(min(ypos) - object@s.misc$axes$dy, length.out = nx), ypos),
        y1 = c(rep(max(ypos) + object@s.misc$axes$dy, length.out = nx), ypos),
        col = grid$col, lwd = grid$lwd, lty = grid$lty)
    
    ## draw ticks
    limis <- current.panel.limits()
    ## if in ptable$x (or y) $tck; number without unit, considered as 'mm', otherwise used with the unit defined by user
    dxticks <- convertHeight(if(is.unit(object@adeg.par$ptable$x$tck)) object@adeg.par$ptable$x$tck else unit(object@adeg.par$ptable$x$tck, "mm"), unitTo = "native", valueOnly = TRUE)
    dyticks <- convertWidth(if(is.unit(object@adeg.par$ptable$y$tck)) object@adeg.par$ptable$y$tck else unit(object@adeg.par$ptable$y$tck, "mm"), unitTo = "native", valueOnly = TRUE)
    ## get parameters
    linespar <- modifyList(as.list(object@trellis.par$axis.line), trellis.par.get()$axis.line, keep.null = TRUE)
    textspar <- modifyList(as.list(object@trellis.par$axis.text), trellis.par.get()$axis.text, keep.null = TRUE)
        
    if(textspar$cex > 0 & dxticks > 0) {
      ## draw ticks for x
      ## same x if top or bottom
      x0axes <- xpos
      ## regular positions
      x1axes <- seq(from = min(xpos), to = max(xpos), length.out = nx)[rank(xpos, ties.method = "first")]
      xxlab <- x1axes
      drawing <- FALSE
      
      if(any(object@adeg.par$ptable$x$pos == "top")) {
        if(any(is.na(object@adeg.par$ptable$x$adj)))
          adj <- c(0, 0.5)
        else
          adj <- object@adeg.par$ptable$x$adj
        y0axes <- limis$ylim[2]
        y1axes <- limis$ylim[2] + dxticks
        if(textspar$cex > 0)
          yylab <- limis$ylim[2] + 1.1 * dxticks
        drawing <- TRUE
      }
      
      if(any(object@adeg.par$ptable$x$pos == "bottom")) {
        if(any(is.na(object@adeg.par$ptable$x$adj)))
          adj <- c(1, 0.5)
        else
          adj <- object@adeg.par$ptable$x$adj
        y0axes <- limis$ylim[1]
        y1axes <- limis$ylim[1] - dxticks
        if(textspar$cex > 0)
          yylab <- limis$ylim[1] - 1.1 * dxticks
        drawing <- TRUE
      }
      
      if(drawing){
        panel.segments(x0 = x0axes, y0 = y0axes, x1 = x1axes, y1 = y1axes,
                       lwd = linespar$lwd, lty = linespar$lty, alpha = linespar$alpha, col = linespar$col)
        if(textspar$cex)
          panel.text(labels = labelsx, x = xxlab, y = yylab, cex = textspar$cex, col = textspar$col, font = textspar$font,
                     lineheight = textspar$lineheight, alpha = textspar$alpha, adj = adj, srt = object@adeg.par$ptable$x$srt)
      }
    }
    
    if(textspar$cex > 0 & dyticks > 0){
      ## draw ticks for y
      y0axes <- ypos
      y1axes <- seq(from = min(ypos),to = max(ypos), length.out = ny)[rank(ypos, ties.method = "first")]
      yylab <- y1axes
      drawing <- FALSE
      
      if(any(object@adeg.par$ptable$y$pos == "right")) {
        if(any(is.na(object@adeg.par$ptable$y$adj)))
          adj <- c(0, 0.5)
        else
          adj <- object@adeg.par$ptable$y$adj
        x0axes <- limis$xlim[2]
        x1axes <- limis$xlim[2] + dyticks
        if(textspar$cex)
          xxlab <- limis$xlim[2] + 1.1 * dyticks
        drawing <- TRUE
      }
      if(any(object@adeg.par$ptable$y$pos == "left")) {
        if(any(is.na(object@adeg.par$ptable$y$adj)))
          adj <- c(1, 0.5)
        else
          adj <- object@adeg.par$ptable$y$adj
        x0axes <- limis$xlim[1]
        x1axes <- limis$xlim[1] - dyticks
        if(textspar$cex)
          xxlab <- limis$xlim[1] - 1.1 * dyticks
        drawing <- TRUE
      }
      if(drawing) {
        panel.segments(x0 = x0axes, y0 = y0axes, x1 = x1axes, y1 = y1axes,
                       lwd = linespar$lwd, lty = linespar$lty, alpha = linespar$alpha, col = linespar$col)
        if(textspar$cex)
          panel.text(labels = labelsy, x = xxlab, y = yylab, cex = textspar$cex, col = textspar$col, font = textspar$font,
                     lineheight = textspar$lineheight, alpha = textspar$alpha, adj = adj, srt = object@adeg.par$ptable$y$srt)
      }
    }
  })


setMethod(
  f = "setlatticecall",
  signature = "ADEg.T",
  definition = function(object){
    name_obj <- deparse(substitute(object))

    ## background and box
    object@trellis.par$panel.background$col <- object@adeg.par$pbackground$col
    if(!object@adeg.par$pbackground$box)
      object@trellis.par$axis.line$col <- "transparent"
    
    arguments <- list(
      par.settings = object@trellis.par,
      scales = list(draw = FALSE),
      panel = function(...) {
        panelbase(object,...)
        panel(object,...)
        
        if(object@adeg.par$plegend$draw)
          setvalueskey(
            method = ifelse(inherits(object, "T.value"), object@g.args$method, "color"),
            breaks = object@s.misc$breaks.update,
            ppoints = object@adeg.par$ppoints,
            plegend = object@adeg.par$plegend,
            symbol = ifelse(inherits(object, "T.value"), object@g.args$symbol, "square"),
            center = object@g.args$center,
            type = "T")
      })
    
    object@lattice.call$arguments <- arguments
    object@lattice.call$graphictype <- "xyplot" 
    patt <- c("main", "sub" , "xlab", "ylab", "xlim", "ylim")
    patt2 <- c("main", "sub" , "xlab", "ylab")
    prese <- pmatch(patt, names(object@g.args))
    prese2 <- pmatch(patt2, names(object@g.args))
    not <- list()
    if(any(is.na(prese2))) {
      not <- not[1:sum(is.na(prese2))]
      names(not) <- patt2[is.na(prese2)]
    }
    object@lattice.call$arguments <- c(object@lattice.call$arguments, object@g.args[patt[which(!is.na(prese))]], not, list(strip = FALSE))
    assign(name_obj, object, envir = parent.frame())
  })


setMethod(
  f = "gettrellis",
  signature = "ADEg.T",
  definition = function(object) {
    if(object@data$storeData) {
      xdata <- object@data$coordsx
      ydata <- object@data$coordsy
    } else {
      xdata <- eval(object@data$coordsx, envir = sys.frame(object@data$frame))
      ydata <- eval(object@data$coordsy, envir = sys.frame(object@data$frame))
    }
    
    tmptrellis <- do.call(what = object@lattice.call$graphictype, args = c(formula(ydata ~ xdata), object@lattice.call$arguments, environment()))
    return(tmptrellis)                            
  })
