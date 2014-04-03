####################################################
##              Curves Plot                       ##
##        1d score represents in 2D plot          ##
####################################################
setClass(
  Class = "ADEg.C1",
  contains = c("ADEg", "VIRTUAL"),
  slots = c(data = "list")
  )

setMethod(
  f = "initialize",
  signature  = "ADEg.C1",
  definition = function(.Object, data = list(score = NULL, frame = 0, storeData = TRUE), ...) {
    ## import the data in @data$score if storeData = TRUE
    .Object <- callNextMethod(.Object, ...) ## ADEg initialize
    if(data$storeData)
      data$score <- eval(data$score, envir = sys.frame(data$frame))
    .Object@data <- data
    return(.Object)
  })


setMethod(
  f = "prepare",
  signature = "ADEg.C1",
  definition = function(object) {
    ## prepare: grid calculations
    ## reset limits and sets axis information for lattice
   
    name_obj <- deparse(substitute(object))
    if(object@data$storeData)
      score <- object@data$score
    else
      score <- eval(object@data$score, envir = sys.frame(object@data$frame))
    
    score <- as.matrix(score)[, 1]  ## to manage 'score' when it is a data.frame with only one column
    
    ## limits and scale
    if(!is.null(object@s.misc$hori.update))
      if(object@s.misc$hori.update != object@adeg.par$p1d$horizontal) {
        aux <- object@g.args$xlim
        object@g.args$xlim <- object@g.args$ylim
        object@g.args$ylim <- aux 
      }
    object@s.misc$hori.update <- object@adeg.par$p1d$horizontal
    
    minX <- min(score)
    maxX <- max(score)
    
    if(object@adeg.par$p1d$horizontal & !is.null(object@g.args$xlim)) {
      minX <- object@g.args$xlim[1]
      maxX <- object@g.args$xlim[2]
    }

    if(!object@adeg.par$p1d$horizontal & !is.null(object@g.args$ylim)) {
      minX <- object@g.args$ylim[1]
      maxX <- object@g.args$ylim[2]
    }

    origin <- object@adeg.par$porigin
    lim <- .setlimits1D(minX, maxX, origin = origin$origin[1], includeOr = origin$include)
    
    ## compute grid size
    tmp <- pretty(lim, n = object@adeg.par$pgrid$nint)
    if(!origin$include)
      origin$origin[1] <- tmp[1]
    
    cgrid <- diff(tmp)[1]
    if(is.na(cgrid))
      stop("error while calculating grid")

    ## compute grid location
    v0 <- origin$origin[1]
    if((origin$origin[1] + cgrid) <= lim[2])
      v0 <- c(v0, seq(origin$origin[1] + cgrid, lim[2], by = cgrid))
    if((origin$origin[1] - cgrid >= lim[1]))
      v0 <- c(v0, seq(origin$origin[1] - cgrid, lim[1], by = -cgrid))
    v0 <- sort(v0[v0 >= lim[1] & v0 <= lim[2]])
    object@s.misc$backgrid <- list(x = v0, d = cgrid)

    scalesandlab <- list()
    if(object@adeg.par$p1d$horizontal) {
      ## draw axes for horizontal plot
      if(object@adeg.par$paxes$draw) {
        scalesandlab$y$draw <- object@adeg.par$paxes$y$draw
        scalesandlab$x <- object@adeg.par$paxes$x
        if(is.null(scalesandlab$x$at))
          scalesandlab$x$at <- object@s.misc$backgrid$x
      } else
        scalesandlab$draw <- FALSE
      if(is.null(object@g.args$xlim))
        object@g.args$xlim <- lim
    } else {
      ## draw axes for vertical plot
      if(object@adeg.par$paxes$draw) {
        scalesandlab$x$draw <- object@adeg.par$paxes$x$draw
        scalesandlab$y <- object@adeg.par$paxes$y
        if(is.null(scalesandlab$y$at))
          scalesandlab$y$at <- object@s.misc$backgrid$x
      } else
        scalesandlab$draw <- FALSE
      if(is.null(object@g.args$ylim))
        object@g.args$ylim <- lim
    }
    
    object@s.misc$scales <- scalesandlab
    assign(name_obj, object, envir = parent.frame())
  })


setMethod(
  f = "panelbase",
  signature = "ADEg.C1",
  definition = function(object, x, y) {
    ## Formula defined in gettrellis
    ## if horizontal, x is score and y is a vector with repetitions of origin
    ## if vertical, this is the inverse
    grid <- object@adeg.par$pgrid
    porigin <- object@adeg.par$porigin 
    pscore <- object@adeg.par$p1d
    lims <- current.panel.limits(unit = "native")
    
    ## for rugs
    if(pscore$rug$draw & (pscore$rug$ticksize != 0)) {
      plines <- object@adeg.par$plines
      if(!is.null(object@data$fac)) {
        ## C1.density or C1.gauss (different colors for rugs)
        if(object@data$storeData)
          fac <- object@data$fac
        else
          fac <- as.factor(eval(object@data$fac, envir = sys.frame(object@data$frame)))
        plines <- lapply(plines, FUN = function(x) {return(rep(x, length.out = nlevels(fac))[fac])})
      }
    }
    lead <- ifelse(pscore$reverse, -1, 1)
         
    if(pscore$horizontal) {
      ## horizontal plot
      
      ## set margins to get some place for rug
      ref <- ifelse(pscore$reverse, lims$ylim[2], lims$ylim[1])
      margin <- ref
      if(pscore$rug$draw)
        margin <- ifelse(is.unit(pscore$rug$margin), convertUnit(pscore$rug$margin, typeFrom = "dimension", unitTo = "native", axisFrom = "y", valueOnly = TRUE), pscore$rug$margin)
            
      ## draw grid
      if(grid$draw)
        panel.segments(x0 = object@s.misc$backgrid$x , x1 = object@s.misc$backgrid$x, y0 = lims$ylim[1], y1 = lims$ylim[2], col = grid$col, lty = grid$lty, lwd = grid$lwd)
      
      ## draw origin
      panel.abline(
        v = if(porigin$draw) porigin$origin else NULL,
        h = if(pscore$rug$draw & pscore$rug$line) ref + lead * margin else NULL,
        col = porigin$col, lwd = porigin$lwd, lty = porigin$lty, alpha = porigin$alpha)
      
      ## draw rug
      if(pscore$rug$draw & (pscore$rug$ticksize != 0)) {
        ## tick end and starting points
        start <- ref + lead * margin
        end <- (start - pscore$rug$ticksize * lead * abs(start - ref))
        start <- convertUnit(unit(start, "native"), unitTo = "npc", axisFrom = "y", valueOnly = TRUE)
        end <- convertUnit(unit(end, "native"), unitTo = "npc", axisFrom = "y", valueOnly = TRUE)
        
        do.call("panel.rug", c(list(x = y, start = start, end = end), plines))
      }
      
    } else {
      ## vertical plot
      
      ## set margins to get some place for rug
      ref <- ifelse(pscore$reverse, lims$xlim[2], lims$xlim[1])
      margin <- ref
      if(pscore$rug$draw)          
        margin <- ifelse(is.unit(pscore$rug$margin), convertUnit(pscore$rug$margin, typeFrom = "dimension", unitTo = "native", axisFrom = "x", valueOnly = TRUE), pscore$rug$margin)
      
      ## draw grid
      if(grid$draw)
        panel.segments(y0 = object@s.misc$backgrid$x , y1 = object@s.misc$backgrid$x, x0 = lims$xlim[1], x1 = lims$xlim[2], col = grid$col, lty = grid$lty, lwd = grid$lwd)

      ## draw origin
      panel.abline(
        h = if(porigin$draw) porigin$origin else NULL,
        v = if(pscore$rug$draw & pscore$rug$line) ref + lead * margin else NULL,
        col = porigin$col, lwd = porigin$lwd, lty = porigin$lty, alpha = porigin$alpha)

      ## draw rug
      if(pscore$rug$draw && pscore$rug$ticksize != 0) {
        ## tick end and starting points
        start <- ref + lead * margin
        end <- (start - pscore$rug$ticksize * lead * abs(start - ref))
        start <- convertUnit(unit(start, "native"), unitTo = "npc", axisFrom = "x", valueOnly =TRUE)
        end <- convertUnit(unit(end, "native"), unitTo = "npc", axisFrom = "x", valueOnly =TRUE)
        do.call("panel.rug", c(list(y = y, start = start, end = end), plines))
      }
    }

    ## indicate grid size (d = **)
    if(grid$draw & (grid$text$cex > 0)) { 
      text.pos <- .setposition(grid$text$pos)
      textgrid <- textGrob(label = paste("d =", object@s.misc$backgrid$d), x = text.pos$posi[1], y = text.pos$posi[2], gp = gpar(cex = grid$text$cex, col = grid$text$col), name = "gridtext")
      grid.rect(x = text.pos$posi[1], y = text.pos$posi[2], width = grobWidth(textgrid), height = grobHeight(textgrid), gp = gpar(fill = object@adeg.par$pbackground$col, alpha = 0.8, col = "transparent"))
      grid.draw(textgrid)
    }
    
    callNextMethod()
  })


setMethod(
  f = "setlatticecall",
  signature = "ADEg.C1",
  definition = function(object) {
    ## arguments recurrents de la liste, pas les limites car elles seront definis ensuite
    name_obj <- deparse(substitute(object))

    ## grid background and box
    object@trellis.par$panel.background$col <- object@adeg.par$pbackground$col
    if(!object@adeg.par$pbackground$box)
      object@trellis.par$axis.line$col <- "transparent"
    
    arguments <- list(
                   par.settings = object@trellis.par,
                   scales = object@s.misc$scales,
                   ## skipt aspect ratio 
                   axis = axis.L, ## see utils.R
                   panel = function(...) {
                     panelbase(object, ...) ## grid,
                     panel(object, ...) ## call to C1.panel function, for slabel and ADEg.C1 class of graphs
                   })
    
    object@lattice.call$arguments <- arguments          
    object@lattice.call$graphictype <- "xyplot" 
    patt <- c("main", "sub", "xlab", "ylab", "xlim", "ylim")
    patt2 <- c("main", "sub", "xlab", "ylab")
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
  signature = "ADEg.C1",
  definition = function(object) {
    if(object@data$storeData)
      score <- object@data$score
    else
      score <- eval(object@data$score, envir = sys.frame(object@data$frame))
    
    score <- as.matrix(score)[, 1]  ## to manage 'score' when it is a data.frame with only one column
    
    if(inherits(object, "C1.barchart"))
      xdata <- 1:length(score)
    else
      xdata <- rep(1, length(score))
    fml <- as.formula(score ~ xdata)
    
    tmptrellis <- do.call(what = object@lattice.call$graphictype, args = c(fml, object@lattice.call$arguments, environment()))
    return(tmptrellis)
  })
