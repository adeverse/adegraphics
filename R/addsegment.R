setMethod(
  f = "addsegment",
  signature = "ADEgORtrellis",
  definition = function(object, x0 = NULL, y0 = NULL, x1, y1, plot = TRUE, ...) {
    
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
    params <- sortparameters$plines
    
    segmentadded <- xyplot(0 ~ 0, xlim = xlim, ylim = ylim, main = NULL, xlab = NULL, ylab = NULL, aspect = aspect,
      panel = function(x, y, ...) panel.segments(x0 = x0, y0 = y0, x1 = x1, y1 = y1, lwd = params$lwd, lty = params$lty, col = params$col), plot = FALSE)
    
    segmentadded$call <- call("xyplot", 0 ~ 0, xlim = substitute(xlim), ylim = substitute(ylim), xlab = NULL, ylab = NULL,
                              aspect = substitute(aspect), lwd = params$lwd, lty = params$lty, col = params$col,
                              x0 = substitute(x0), y0 = substitute(y0), x1 = substitute(x1), y1 = substitute(y1),
                              panel = function(x, y, ...) panel.segments(x0 = x0, y0 = y0, x1 = x1, y1 = y1))
    
    # superposition
    obj <- superpose(object, segmentadded, plot = FALSE)
    nn <- all.names(substitute(object))
    names(obj) <- c(ifelse(is.na(nn[2]), nn[1], nn[2]), "segmentadded")
    
    if(plot)
      print(obj)
    invisible(obj)
  })

