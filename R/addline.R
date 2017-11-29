setMethod(
  f = "addline",
  signature = "ADEg",
  definition = function(object, a = NULL, b = 0, h = NULL, v = NULL, plot = TRUE, ...) {
    
    # collect limits
    xlim <- object@g.args$xlim
    ylim <- object@g.args$ylim
    aspect <- object@adeg.par$paxes$aspectratio
    
    ## sorting parameters
    sortparameters <- sortparamADEg(...)$adepar
    params <- adegpar()
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    params <- sortparameters$plines
    
    lineadded <- xyplot(0 ~ 0, xlim = xlim, ylim = ylim, xlab = NULL, ylab = NULL, aspect = aspect, 
                        panel = function(x, y, ...) panel.abline(a = a, b = b, h = h, v = v, lwd = params$lwd, lty = params$lty, col = params$col), plot = FALSE)
    
    lineadded$call <- call("xyplot", 0 ~ 0, xlim = substitute(xlim), ylim = substitute(ylim), xlab = NULL, ylab = NULL,
                           aspect = substitute(aspect), lwd = params$lwd, lty = params$lty, col = params$col,
                           a = substitute(a), b = substitute(b), h = substitute(h), v = substitute(v),
                           panel = function(x, y, ...) panel.abline(a = a, b = b, h = h, v = v))
    
    # superposition
    obj <- superpose(object, lineadded, plot = FALSE)
    nn <- all.names(substitute(object))
    names(obj) <- c(ifelse(is.na(nn[2]), nn[1], nn[2]), "lineadded")
    
    if(plot)
      print(obj)
    invisible(obj)
  })
