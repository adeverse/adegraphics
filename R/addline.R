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
    
    lineadded <- xyplot(0 ~ 0, xlim = xlim, ylim = ylim, xlab = NULL, ylab = NULL, aspect = aspect, mya = a, myb = b, myh = h, myv = v, 
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


setMethod(
  f = "addline",
  signature = "ADEgS",
  definition = function(object, a = NULL, b = 0, h = NULL, v = NULL, plot = TRUE, which = 1:length(object), ...) {
    ngraph <- length(object)
    if(max(which) > ngraph)
      stop("Values in 'which' should be lower than the length of object")
    
    if(length(which) == 1) {
      object[[which]] <- addline(object[[which]], a = a, b = b, h = h, v = v, ..., plot = FALSE)
      
    } else {
      
      if(sum(object@add) != 0)
        stop("The 'addline' function is not available for superposed objects.", call. = FALSE)
      
      ## sorting parameters
      sortparameters <- sortparamADEg(...)$adepar
      params <- adegpar()
      sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
      params <- sortparameters$plines
      params <- rapply(params, function(X) rep(X, length.out = length(which)), how = "list")
      
      if(!is.null(a)) a <- rep_len(a, length.out = length(which))
      b <- rep_len(b, length.out = length(which))
      if(!is.null(h)) h <- rep_len(h, length.out = length(which))
      if(!is.null(v)) v <- rep_len(v, length.out = length(which))
      
      for (i in which)
        object[[i]] <- addline(object[[i]], a = a[i], b = b[i], h = h[i], v = v[i], which = 1, plot = FALSE, plines = lapply(params, function(X) X[i]))
    }
    
    obj <- object
    if(plot)
      print(obj)
    invisible(obj)
  })