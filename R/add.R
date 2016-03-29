setMethod(
  f = "addtext",
  signature = "ADEgORtrellis",
  definition = function(object, xcoord, ycoord, label, plot = TRUE, ...) {
    
    size <- max(length(xcoord), length(ycoord), length(label))
    xcoord <- rep_len(xcoord, length.out = size)
    ycoord <- rep_len(ycoord, length.out = size)
    labels <- rep_len(label, length.out = size)
    
    ## sorting parameters
    sortparameters <- sortparamADEg(...)$adepar
    params <- adegpar()
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    params <- sortparameters$plabels
    
    if(inherits(object, "ADEg")) {
      xlim <- object@g.args$xlim
      ylim <- object@g.args$ylim
    } else {
      xlim <- object$x.limits
      ylim <- object$y.limits
    }

    textadded <- xyplot(ycoord ~ xcoord, panel = function(x, y, ...)
        adeg.panel.label(x, y, labels, plabels = params), plot = FALSE)

    textadded$call <- call("xyplot", ycoord ~ xcoord, xlim = substitute(xlim), ylim = substitute(ylim), labels = substitute(labels), 
                
                           panel = function(x, y, labels, ...) adeg.panel.label(x, y, labels = labels, plabels = params))
    
    obj <- superpose(object, textadded, plot = FALSE)
    names(obj) <- c("object", "textadded")
    
    if(plot)
      print(obj)
    invisible(obj)
  })


setMethod(
  f = "addtext",
  signature = "ADEgS",
  definition = function(object, xcoord, ycoord, label, plot = TRUE, which = 1:length(object),...) {
    ngraph <- length(object)
    if(max(which) > ngraph)
      stop("Values in 'which' should be lower than the length of object")
    
    
    if(length(which) == 1) { # if only one subgraph is selected, all the labels are displayed on this unique subgraph
      size <- max(length(xcoord), length(ycoord), length(label))
      xcoord <- rep_len(xcoord, length.out = size)
      ycoord <- rep_len(ycoord, length.out = size)
      labels <- rep_len(label, length.out = size)
      
      object[[which]] <- addtext(object[[which]], xcoord, ycoord, labels, ..., which = 1, plot = FALSE)
      
    } else { # if several subgraphs are selected, each label is displayed on one subgraph; there is only one label by subgraph
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