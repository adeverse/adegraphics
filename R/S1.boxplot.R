###########################################################
##                          s1d.boxplot                  ##
###########################################################

setClass(
  Class = "S1.boxplot",
  contains = "ADEg.S1",
  )


setMethod(
  f = "initialize",
  signature  = "S1.boxplot",
  definition = function(.Object, data = list(score = NULL, fac = NULL, frame = 0, storeData = TRUE), ...) {
    .Object <- callNextMethod(.Object, data = data, ...) ## ADEg.S1 initialize
    if(data$storeData)
      data$fac <- eval(data$fac, envir = sys.frame(data$frame))
    .Object@data$fac <- data$fac
    return(.Object)
  })


setMethod(
  f = "prepare",
  signature = "S1.boxplot",
  definition = function(object) {
    name_obj <- deparse(substitute(object))
    if(object@data$storeData)
      fac <- as.factor(object@data$fac)
    else
      fac <- as.factor(eval(object@data$fac, envir = sys.frame(object@data$frame)))
    
    ## pre-management of graphics parameters      
    oldparamadeg <- adegpar()
    on.exit(adegpar(oldparamadeg))
    adegtot <- adegpar(object@adeg.par)
    
    ## change default for some parameters
    if(adegtot$p1d$horizontal & is.null(object@adeg.par$plabels$orientation))
      adegtot$plabels$orientation <- 0
    else if(!adegtot$p1d$horizontal & is.null(object@adeg.par$plabels$orientation))
      adegtot$plabels$orientation <- 90
    if(is.null(object@adeg.par$ppolygons$col))
      adegtot$ppolygons$col <- "white"

    if(!is.null(object@g.args$col))
      if(is.logical(object@g.args$col)) {
        if(object@g.args$col)
        	adegtot$ppoints$col <- adegtot$ppoints$fill <- adegtot$plabels$col <- adegtot$plabels$boxes$border <- adegtot$plines$col <- adegtot$ppolygons$border <- adegtot$ppolygons$col <- adegtot$ppalette$quali(nlevels(fac))
      } else
        adegtot$ppoints$col <- adegtot$ppoints$fill <- adegtot$plabels$col <- adegtot$plabels$boxes$border <- adegtot$plines$col <- adegtot$ppolygons$border <- adegtot$ppolygons$col <- rep(object@g.args$col, length.out = nlevels(fac))
    
    ## object modification before calling inherited method
    object@adeg.par <- adegtot
    callNextMethod() ## prepare graph
    
    assign(name_obj, object, envir = parent.frame())
  })


setMethod(
  f = "S1.panel",
  signature = "S1.boxplot",
  definition = function(object, x, y) {

    if(object@data$storeData)
      fac <- object@data$fac
    else
      fac <- eval(object@data$fac, envir = sys.frame(object@data$frame))

    fac <- as.factor(fac)
    nlev <- nlevels(fac)
    labels <- levels(fac)
    
    lims <- current.panel.limits(unit = "native")
    p1d <- object@adeg.par$p1d
    lead <- ifelse(p1d$reverse, -1, 1)
    plabels <- object@adeg.par$plabels
    
    ## repeat graphical parameters (one for each level)
    ppoints <- lapply(object@adeg.par$ppoints, FUN = function(x) x <- rep(x, length.out = nlev))
    ppoints <- lapply(ppoints, FUN = function(x) x <- x[1:nlev])
    plines <- lapply(object@adeg.par$plines, FUN = function(x) x <- rep(x, length.out = nlev))
    plines <- lapply(plines, FUN = function(x) x <- x[1:nlev])
    ppolygons <- lapply(object@adeg.par$ppolygons, FUN = function(x) x <- rep(x, length.out = nlev))
    ppolygons <- lapply(ppolygons, FUN = function(x) x <- x[1:nlev])
    
    ## manage trellis parameters
    oldcolsymbol <- trellis.par.get("plot.symbol")$col
    oldcolumbrella <- trellis.par.get("box.umbrella")$col
    oldcolrectangle <- trellis.par.get("box.rectangle")$col
    trellis.par.set(list("plot.symbol" = list("col" = "black"), "box.umbrella" = list("col" = plines$col), "box.rectangle" = list("col" = ppolygons$border)))
    on.exit(trellis.par.set(list("plot.symbol" = list("col" = oldcolsymbol), "box.umbrella" = list("col" = oldcolumbrella), "box.rectangle" = list("col" = oldcolrectangle))))

    ## manage string rotation
    srt <- 0
    if(is.numeric(plabels$orientation[1]))
      srt <- plabels$orientation[1]
    else {
      if(plabels$orientation[1] == "horizontal")
        srt <- 0
      else if(plabels$orientation[1] == "vertical")
        srt <- 90
    }
    
    gettextpos <- function(x, lim) {
      if(length(x) != 2) {
        ## if no data in the given level
        return(c(NA, NA))
      } else {
        if(abs(lim[2] - x[2]) > abs(lim[1] - x[1]))
          return(c(x[2], 1))
        else
          return(c(x[1], -1))
      }
    }
    
    if(p1d$horizontal) {
      ## horizontal plot
      ref <- if(p1d$reverse) rev(lims$ylim) else lims$ylim
      margin <- ref[1]
      if(p1d$rug$draw)
        margin <- ifelse(is.unit(p1d$rug$margin), convertUnit(p1d$rug$margin, typeFrom = "dimension", unitTo = "native", axisFrom = "y", valueOnly = TRUE), p1d$rug$margin)
      
      ylab <- seq(from = ref[1] + lead * margin, to = ref[2], length.out = nlev + 2)
      ylab <- ylab[-c(1, length(ylab))]
      bwid <- diff(range(ylab)) / (nlev + 1)

      ## panel.bwplot
      do.call("panel.bwplot", list(x = y, y = ylab[fac], box.ratio = bwid, coef = 1.5, pch = "|", horizontal = TRUE))
      
      ## add means
      do.call("panel.points", c(list(x = (tapply(y, fac, mean)), y = ylab), ppoints))
      minmax <- tapply(y, fac, range)
      etis <- sapply(minmax, gettextpos, lim = lims$xlim)
      
    } else {
      ## vertical plot
      ref <- if(p1d$reverse) rev(lims$xlim) else lims$xlim
      margin <- ref[1]
      if(p1d$rug$draw)
        margin <- ifelse(is.unit(p1d$rug$margin), convertUnit(p1d$rug$margin, typeFrom = "dimension", unitTo = "native", axisFrom = "x", valueOnly = TRUE), p1d$rug$margin)
      
      xlab <- seq(from = ref[1] + lead * margin, to = ref[2], length.out = nlev + 2)
      xlab <- xlab[-c(1, length(xlab))]
      bwid <- diff(range(xlab)) / (nlev + 1)
      
      ## panel.bwplot
      do.call("panel.bwplot", list(x = xlab[fac], y = y, box.ratio = bwid, coef = 1.5, pch = "|", horizontal = FALSE))
      
      ## add means
      do.call("panel.points", c(list(y = (tapply(y, fac , mean)), x = xlab), ppoints))
      minmax <- tapply(y, fac, range)
      etis <- sapply(minmax, gettextpos, lim = lims$ylim)
     }
    
    ## draw labels
    if(abs(sin(srt)) > sin(45)) {
      ## almost vertical labels
      if(p1d$horizontal)
        width <- stringWidth("h")
      else
        width <- stringWidth(labels) + stringWidth("h")
      
      width <- rep(plabels$cex, length.out = length(labels)) * convertUnit(width, "native", typeFrom = "dimension", axisFrom = "x", axisTo = "y", valueOnly = TRUE) / 2 
    } else {
      ## almost horizontal labels
      if(p1d$horizontal)
        width <- stringWidth(labels) + stringWidth("h")
      else
        width <- stringWidth("h")
      
      width <- rep(plabels$cex, length.out = length(labels)) * convertUnit(width, "native", typeFrom = "dimension", axisFrom = "x", valueOnly = TRUE) / 2 
    }
    
    if(p1d$horizontal)
      adeg.panel.label(x = etis[1, ] + etis[2, ] * width, y = ylab, labels = labels, plabels = plabels)
    else
      adeg.panel.label(x = xlab, y = etis[1, ] + etis[2, ] * width, labels = labels, plabels = plabels)
  })

  
## For  boxplot, parameters can only be changed using par.settings arguments;
setMethod(
  f = "setlatticecall",
  signature = "S1.boxplot",
  definition = function(object) {
    name_obj <- deparse(substitute(object))
    callNextMethod()
    ppolygons <- object@adeg.par$ppolygons
    object@lattice.call$arguments$par.settings <- modifyList(list(box.rectangle = c(list(col = ppolygons$border, fill = ppolygons$col), 
    	ppolygons[-c(which(names(ppolygons) == "border" | (names(ppolygons) == "col")))]), box.umbrella = object@adeg.par$plines, 
      plot.symbol = modifyList(list(col = "black", fill = "black"), object@adeg.par$ppoints)), object@lattice.call$arguments$par.settings, keep.null = TRUE)
    assign(name_obj, object, envir = parent.frame())
  })


s1d.boxplot <- function(score, fac, col = NULL, facets = NULL, plot = TRUE, storeData = FALSE, add = FALSE, pos = -1, ... ) {

  ## evaluation of some parameters
  thecall <- .expand.call(match.call())
  fac <- eval(thecall$fac, envir = sys.frame(sys.nframe() + pos))
  score <- eval(thecall$score, envir = sys.frame(sys.nframe() + pos))
  if(NROW(fac) != NROW(score))
    stop("fac and score must have the same number of rows")
  
  ## parameters sorted
  sortparameters <- .specificpar(...)
  
  ## facets
  if(!is.null(facets)) {
    if(NCOL(score) == 1 & NCOL(fac) == 1)
      object <- multi.facets.S1(thecall, sortparameters$adepar, samelimits = sortparameters$g.args$samelimits)
    else 
      stop("Facets are not allowed with multiple scores or fac")
  }
  
  ## multiple scores
  else if(NCOL(score) > 1) {
    if(NCOL(fac) == 1)
      object <- multi.score.S1(thecall)
    else 
      stop("Multiple scores are not allowed with multiple fac")
  }
  
  ## multiple fac
  else if(NCOL(fac) > 1) {
    object <- multi.variables.S1(thecall, "fac")
  }
  
  ## simple ADEg graphic
  else {
    if(length(sortparameters$rest))
      warning(c("Unused parameters: ", paste(unique(names(sortparameters$rest)), " ", sep = "")), call. = FALSE)
    
    ## creation of the ADEg object
    g.args <- c(sortparameters$g.args, list(col = col))
    tmp_data <- list(score = thecall$score, fac = fac, frame = sys.nframe() + pos, storeData = storeData)
    object <- new(Class = "S1.boxplot", data = tmp_data, adeg.par = sortparameters$adepar, trellis.par = sortparameters$trellis, g.args = g.args, Call = match.call())
    
    ## preparation
    prepare(object) 
    setlatticecall(object)
    if(add)
      object <- add.ADEg(object)
  }
  if(!add & plot)
    print(object)
  invisible(object)
}
 