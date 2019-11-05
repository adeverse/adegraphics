setMethod(
  f = "addtext",
  signature = "ADEg",
  definition = function(object, xcoord, ycoord, label, plot = TRUE, ...) {
    
    # iterate coordinates and/or labels if necessary
    size <- max(length(xcoord), length(ycoord), length(label))
    xcoord <- rep_len(xcoord, length.out = size)
    ycoord <- rep_len(ycoord, length.out = size)
    labels <- rep_len(label, length.out = size)
    
    # collect limits
    xlim <- object@g.args$xlim
    ylim <- object@g.args$ylim
    aspect <- object@adeg.par$paxes$aspectratio
    
    ## sorting parameters
    sortparameters <- sortparamADEg(...)$adepar
    params <- adegpar()
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    params <- sortparameters$plabels
    
    # create the lattice object
    textadded <- xyplot(ycoord ~ xcoord, xlim = xlim, ylim = ylim, xlab = NULL, ylab = NULL, aspect = aspect,
                        panel = function(x, y, ...) adeg.panel.label(x, y, labels, plabels = params), plot = FALSE)
    
    textadded$call <- call("xyplot", ycoord ~ xcoord, xlim = substitute(xlim), ylim = substitute(ylim), xlab = NULL, ylab = NULL,
                           aspect = substitute(aspect), labels = substitute(labels), 
                           panel = function(x, y, labels, ...) adeg.panel.label(x, y, labels = labels, plabels = params))
    
    # superposition
    obj <- superpose(object, textadded, plot = FALSE)
    nn <- all.names(substitute(object))
    names(obj) <- c(ifelse(is.na(nn[2]), nn[1], nn[2]), "textadded")
    
    if(plot)
      print(obj)
    invisible(obj)
  })

setMethod(
  f = "addtext",
  signature = "trellis",
  definition = function(object, xcoord, ycoord, label, plot = TRUE, ...) {
    
    # iterate coordinates and/or labels if necessary
    size <- max(length(xcoord), length(ycoord), length(label))
    xcoord <- rep_len(xcoord, length.out = size)
    ycoord <- rep_len(ycoord, length.out = size)
    labels <- rep_len(label, length.out = size)
    
    # collect limits
    xlim <- c(0,1)
    ylim <- c(0,1)
    if (is.numeric(object$x.limits))
      xlim <- object$x.limits
    if (is.numeric(object$y.limits))
      ylim <- object$y.limits

    aspect <- object$aspect.ratio
    
    ## sorting parameters
    sortparameters <- sortparamADEg(...)$adepar
    params <- adegpar()
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    params <- sortparameters$plabels
    
    # create the lattice object
    textadded <- xyplot(ycoord ~ xcoord, xlim = xlim, ylim = ylim, xlab = NULL, ylab = NULL, aspect = aspect,
      panel = function(x, y, ...) adeg.panel.label(x, y, labels, plabels = params), plot = FALSE)
    
    textadded$call <- call("xyplot", ycoord ~ xcoord, xlim = substitute(xlim), ylim = substitute(ylim), xlab = NULL, ylab = NULL,
      aspect = substitute(aspect), labels = substitute(labels), 
      panel = function(x, y, labels, ...) adeg.panel.label(x, y, labels = labels, plabels = params))
    
    # superposition
    obj <- superpose(object, textadded, plot = FALSE)
    nn <- all.names(substitute(object))
    names(obj) <- c(ifelse(is.na(nn[2]), nn[1], nn[2]), "textadded")
    
    if(plot)
      print(obj)
    invisible(obj)
  })

setMethod(
  f = "addtext",
  signature = "ADEgS",
  definition = function(object, xcoord, ycoord, label, plot = TRUE, which = 1:length(object), ...) {
    ngraph <- length(object)
    if(max(which) > ngraph)
      stop("Values in 'which' should be lower than the length of object")
    
    if(length(which) == 1) { # if only one subgraph is selected, all the labels are displayed on this unique subgraph
      size <- max(length(xcoord), length(ycoord), length(label))
      xcoord <- rep_len(xcoord, length.out = size)
      ycoord <- rep_len(ycoord, length.out = size)
      labels <- rep_len(label, length.out = size)
      
      object[[which]] <- addtext(object[[which]], xcoord, ycoord, labels, ..., plot = FALSE)
      
    } else { # if several subgraphs are selected, each label is displayed on one subgraph; there is only one label by subgraph

      if(sum(object@add) != 0)
        stop("The 'addtext' function is not available for superposed objects.", call. = FALSE)
      
      ## sorting parameters
      sortparameters <- sortparamADEg(...)$adepar
      params <- adegpar()
      sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
      params <- sortparameters$plabels
      params <- rapply(params, function(X) rep(X, length.out = length(which)), how = "list")
      
      xcoord <- rep_len(xcoord, length.out = length(which))
      ycoord <- rep_len(ycoord, length.out = length(which))
      labels <- rep_len(label, length.out = length(which))
      
      for (i in which)
        object[[i]] <- addtext(object[[i]], xcoord[i], ycoord[i], labels[i], which = 1, plot = FALSE, plabels = lapply(params, function(X) X[i]))
    }
    
    obj <- object
    if(plot)
      print(obj)
    invisible(obj)
  })