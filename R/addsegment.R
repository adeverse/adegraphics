setMethod(
  f = "addsegment",
  signature = "ADEg",
  definition = function(object, x0 = NULL, y0 = NULL, x1, y1, plot = TRUE, ...) {
    
    # collect limits
    xlim <- object@g.args$xlim
    ylim <- object@g.args$ylim
    aspect <- object@adeg.par$paxes$aspectratio
    
    ## sorting parameters
    sortparameters <- sortparamADEg(...)$adepar
    params <- adegpar()
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    params <- sortparameters$plines
    
    segmentadded <- xyplot(0 ~ 0, xlim = xlim, ylim = ylim, main = NULL, xlab = NULL, ylab = NULL, aspect = aspect, 
                           myx0 = x0, myy0 = y0, myx1 = x1, myy1 = y1, 
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

setMethod(
  f = "addsegment",
  signature = "ADEgS",
  definition = function(object, x0 = NULL, y0 = NULL, x1, y1, plot = TRUE, which = 1:length(object), ...) {
    ngraph <- length(object)
    if(max(which) > ngraph)
      stop("Values in 'which' should be lower than the length of object")
    
    if(length(which) == 1) {
      object[[which]] <- addsegment(object[[which]], x0 = x0, y0 = y0, x1 = x1, y1 = y1, ..., plot = FALSE)
      
    } else {
      
      if(sum(object@add) != 0)
        stop("The 'addsegment' function is not available for superposed objects.", call. = FALSE)
      
      ## sorting parameters
      sortparameters <- sortparamADEg(...)$adepar
      params <- adegpar()
      sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
      params <- sortparameters$plines
      params <- rapply(params, function(X) rep(X, length.out = length(which)), how = "list")
      
      if(!is.null(x0)) x0 <- rep_len(x0, length.out = length(which))
      if(!is.null(y0)) y0 <- rep_len(y0, length.out = length(which))
      x1 <- rep_len(x1, length.out = length(which))
      y1 <- rep_len(y1, length.out = length(which))
      
      for (i in which)
        object[[i]] <- addsegment(object[[i]], x0 = x0[i], y0 = y0[i], x1 = x1[i], y1 = y1[i], which = 1, plot = FALSE, plines = lapply(params, function(X) X[i]))
    }
    
    obj <- object
    if(plot)
      print(obj)
    invisible(obj)
  })