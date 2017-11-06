setMethod(
  f = "addpoint",
  signature = "ADEgORtrellis",
  definition = function(object, xcoord, ycoord, plot = TRUE, ...) {
    
    # iterate coordinates and/or labels if necessary
    size <- max(length(xcoord), length(ycoord))
    xcoord <- rep_len(xcoord, length.out = size)
    ycoord <- rep_len(ycoord, length.out = size)
    
    # collect limits
    if(inherits(object, "ADEg")) {
      xlim <- object@g.args$xlim
      ylim <- object@g.args$ylim
      aspect <- object@adeg.par$paxes$aspectratio
    } else {
      xlim <- object$x.limits
      ylim <- object$y.limits
      aspect <- object$aspect.ratio
    }
    
    ## sorting parameters
    sortparameters <- sortparamADEg(...)$adepar
    params <- adegpar()
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    params <- sortparameters$ppoints
    
    # create the lattice object
    pointadded <- xyplot(ycoord ~ xcoord, xlim = xlim, ylim = ylim, xlab = NULL, ylab = NULL, aspect = aspect, 
      panel = function(x, y, ...) panel.points(xcoord, ycoord, alpha = params$alpha, cex = params$cex, col = params$col, pch = params$pch, fill = params$fill), plot = FALSE)
    
    pointadded$call <- call("xyplot", ycoord ~ xcoord, xlim = substitute(xlim), ylim = substitute(ylim), xlab = NULL, ylab = NULL,
                            aspect = substitute(aspect), alpha = params$alpha, cex = params$cex, col = params$col, pch = params$pch, fill = params$fill,
                            panel = function(x, y, ...) panel.abline(x, y))
    
    # superposition
    obj <- superpose(object, pointadded, plot = FALSE)
    nn <- all.names(substitute(object))
    names(obj) <- c(ifelse(is.na(nn[2]), nn[1], nn[2]), "pointadded")
    
    if(plot)
      print(obj)
    invisible(obj)
  })

