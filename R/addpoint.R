setMethod(
  f = "addpoint",
  signature = "ADEg",
  definition = function(object, xcoord, ycoord, plot = TRUE, ...) {
    
    # iterate coordinates if necessary
    size <- max(length(xcoord), length(ycoord))
    xcoord <- rep_len(xcoord, length.out = size)
    ycoord <- rep_len(ycoord, length.out = size)
    
    # collect limits
    xlim <- object@g.args$xlim
    ylim <- object@g.args$ylim
    aspect <- object@adeg.par$paxes$aspectratio
    
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


setMethod(
  f = "addpoint",
  signature = "ADEgS",
  definition = function(object, xcoord, ycoord, plot = TRUE, which = 1:length(object), ...) {
    ngraph <- length(object)
    if(max(which) > ngraph)
      stop("Values in 'which' should be lower than the length of object")
    
    if(length(which) == 1) {
      size <- max(length(xcoord), length(ycoord))
      xcoord <- rep_len(xcoord, length.out = size)
      ycoord <- rep_len(ycoord, length.out = size)
      
      object[[which]] <- addpoint(object[[which]], xcoord, ycoord, ..., plot = FALSE)
      
    } else {
      
      if(sum(object@add) != 0)
        stop("The 'addpoint' function is not available for superposed objects.", call. = FALSE)
      
      ## sorting parameters
      sortparameters <- sortparamADEg(...)$adepar
      params <- adegpar()
      sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
      params <- sortparameters$ppoints
      params <- rapply(params, function(X) rep(X, length.out = length(which)), how = "list")
      
      xcoord <- rep_len(xcoord, length.out = length(which))
      ycoord <- rep_len(ycoord, length.out = length(which))
      
      for (i in which)
        object[[i]] <- addpoint(object[[i]], xcoord[i], ycoord[i], which = 1, plot = FALSE, ppoints = lapply(params, function(X) X[i]))
    }
    
    obj <- object
    if(plot)
      print(obj)
    invisible(obj)
  })