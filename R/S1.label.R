###########################################################
##                          s1d.label                    ##
###########################################################

setClass(
  Class = "S1.label",
  contains = "ADEg.S1"
)


setMethod(
  f = "initialize",
  signature = "S1.label",
  definition = function(.Object, data = list(score = NULL, labels = NULL, frame = 0, storeData = TRUE), ...) {
    .Object <- callNextMethod(.Object, data = data, ...) ## ADEg.C1 initialize
    if(data$storeData)
      data$labels <- eval(data$labels, envir = sys.frame(data$frame))
    .Object@data$labels <- data$labels
    validObject(.Object)
    return(.Object)
  })


setMethod(
  f = "prepare",
  signature = "S1.label",
  definition = function(object) {
    name_obj <- deparse(substitute(object))
    
    ## pre-management of graphics parameters      
    oldparamadeg <- adegpar()
    on.exit(adegpar(oldparamadeg))
    adegtot <- adegpar(object@adeg.par)
    
    ## change default for some parameters
    if(adegtot$p1d$horizontal & is.null(object@adeg.par$plabels$orientation))
      adegtot$plabels$orientation <- 90
    else if(!adegtot$p1d$horizontal & is.null(object@adeg.par$plabels$orientation))
      adegtot$plabels$orientation <- 0
    
    ## object modification before calling inherited method
    object@adeg.par <- adegtot
    callNextMethod() ## prepare graph
    
    assign(name_obj, object, envir = parent.frame())
  })


setMethod(
  f = "S1.panel",
  signature = "S1.label",
  definition = function(object, x, y) {

    lims <- current.panel.limits(unit = "native")
    pscore <- object@adeg.par$p1d
    plabels <- object@adeg.par$plabels
    plboxes <- plabels$boxes

    if(object@data$storeData)
      labels <- object@data$labels
    else
      labels <- eval(object@data$labels, envir = sys.frame(object@data$frame))
    
    nval <- length(y)
    
    if(!is.null(labels)) {
      ## get text sizes for boxes
      test <- .textsize(labels, plabels)
      srt <- test$srt
      w <- test$w
      h <- test$h
    }        

    lead <- ifelse(pscore$reverse, -1, 1)

    if(pscore$horizontal) {
      ## horizontal plot
      xpoints <- y
      ## get positions for labels
      if(object@g.args$poslabel == "regular") {
        spacelab <- diff(lims$xlim) / (nval + 1)
        xlab <- seq(from = lims$xlim[1] + spacelab, by = spacelab, length.out = nval)[rank(xpoints, ties.method = "first")]
      } else
        xlab <- xpoints
      
      ## set margins to get some place for rug
      ref <- ifelse(pscore$reverse, lims$ylim[2], lims$ylim[1])
      margin <- ref
      if(pscore$rug$draw)
        margin <- ifelse(is.unit(pscore$rug$margin), convertUnit(pscore$rug$margin, typeFrom = "dimension", unitTo = "native", axisFrom = "y", valueOnly = TRUE), pscore$rug$margin)
      ypoints <- ref + lead * margin
      ylab <- ypoints + lead * pscore$labpos

      ## draw segments and labels
      if(pscore$labpos != 0)
        do.call("panel.segments", c(list(x0 = xpoints, y0 = ypoints , y1 = ylab, x1 = xlab), object@adeg.par$plines))
      if(!is.null(labels) & any(plabels$cex > 0))
        adeg.panel.label(x = xlab , y = ylab + lead * h / 2, labels = labels, plabels = plabels)
    } else {
      ## vertical plot
      ypoints <- y
      ## get positions for labels
      if(object@g.args$poslabel == "regular") {
        spacelab <- diff(lims$ylim) / (nval + 1)
        ylab <- seq(from = lims$ylim[1] + spacelab, by = spacelab, length.out = nval)[rank(ypoints, ties.method = "first")]
      } else
        ylab <- ypoints
      
      ## set margins to get some place for rug
      ref <- ifelse(pscore$reverse, lims$xlim[2], lims$xlim[1])
      margin <- ref
      if(pscore$rug$draw)
        margin <- ifelse(is.unit(pscore$rug$margin), convertUnit(pscore$rug$margin, typeFrom = "dimension", unitTo = "native", axisFrom = "x", valueOnly = TRUE), pscore$rug$margin)
      xpoints <- ref + lead * margin
      xlab <- xpoints + lead * pscore$labpos
      
      ## draw segments and labels
      if(pscore$labpos != 0)
        do.call("panel.segments", c(list(x0 = xpoints, y0 = ypoints, y1 = ylab, x1 = xlab), object@adeg.par$plines))
      
      if(!is.null(labels) & any(plabels$cex > 0))
        adeg.panel.label(x = xlab + lead * w / 2 , y = ylab, labels = labels, plabels = plabels)
    }
    if(any(object@adeg.par$ppoints$cex > 0))
      do.call("panel.points", c(list(x = xpoints, y = ypoints), object@adeg.par$ppoints))
  })


s1d.label <- function(score, labels = 1:NROW(score), poslabel = c("regular", "value"), facets = NULL, plot = TRUE, storeData = FALSE, add = FALSE, pos = -1, ...) {
  
  ## evaluation of some parameters
  thecall <- .expand.call(match.call())
  score <- eval(thecall$score, envir = sys.frame(sys.nframe() + pos))
  
  ## parameters sorted
  sortparameters <- .specificpar(...)
  
  ## facets
  if(!is.null(facets)) {
    if(NCOL(score) == 1)
      object <- multi.facets.S1(thecall, sortparameters$adepar, samelimits = sortparameters$g.args$samelimits)
    else 
      stop("Facets are not allowed with multiple scores")
  }
  
  ## multiple scores
  else if(NCOL(score) > 1) { 
    object <- multi.score.S1(thecall)
  }
  
  ## simple ADEg graphic
  else {
    if(length(sortparameters$rest))
      warning(c("Unused parameters: ", paste(unique(names(sortparameters$rest)), " ", sep = "")), call. = FALSE)
    
    ## creation of the ADEg object
    g.args <- c(sortparameters$g.args, list(poslabel = match.arg(poslabel)))
    tmp_data <- list(score = thecall$score, labels = thecall$labels, frame = sys.nframe() + pos, storeData = storeData)
    object <- new(Class = "S1.label", data = tmp_data, adeg.par = sortparameters$adepar, trellis.par = sortparameters$trellis, g.args = g.args, Call = match.call())
    
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
