setClass(
  Class = "C1.curves",
  contains = "C1.curve"
)

setMethod(
  f = "panel",
  signature = "C1.curves",
  definition = function(object, x, y) {
    ## Drawing dotchart
    ## x is the index
    ## y is the score
    
    ## get some parameters
    nr <- NROW(object@data$score)
    nc <- NCOL(object@data$score)
    
    pscore <- object@adeg.par$p1d
    ppoints <- lapply(object@adeg.par$ppoints, FUN = function(x) {rep(rep(x, length.out = nc), each = nr)})
    plines <- lapply(object@adeg.par$plines, FUN = function(x) {rep(rep(x, length.out = nc), each = nr)})
    
    ymat <- matrix(y, nrow =  nr, ncol = nc)
    ## reorder the values
    y <- as.vector(ymat[order(x), ])
    x <- sort(x)
    
    ## Starts the display
    ## depends on the parametres horizontal
    ## rug.draw and reverse are always considered as FALSE
    
    for(i in 1:nc){
      idx <- (i - 1)*nr + (1:nr)
      if(pscore$horizontal) {
        x.tmp <- y[idx]
        y.tmp <- x
      } else {
        x.tmp <- x
        y.tmp <- y[idx]
      }
      
      panel.lines(x = x.tmp, y = y.tmp, lwd = plines$lwd[idx], lty = plines$lty[idx], col = plines$col[idx])
      panel.points(x = x.tmp, y = y.tmp, pch = ppoints$pch[idx], cex = ppoints$cex[idx], col = ppoints$col[idx], alpha = ppoints$alpha[idx])
    }
  })


s1d.curves <- function(score, at = 1:NROW(score), facets = NULL, plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...) {
  
  ## evaluation of some parameters
  thecall <- .expand.call(match.call())
  ## parameters sorted
  sortparameters <- sortparamADEg(...)
  
  ## facets
  if(!is.null(facets)) {
    if(NCOL(score) == 1)
      object <- multi.facets.C1(thecall, sortparameters$adepar, samelimits = sortparameters$g.args$samelimits)
    else 
      stop("Facets are not allowed with multiple scores")
  }
  
  ## simple ADEg graphic
  else {
    if(length(sortparameters$rest))
      warning(c("Unused parameters: ", paste(unique(names(sortparameters$rest)), " ", sep = "")), call. = FALSE)
    
    ## creation of the ADEg object
    if(storeData)
      tmp_data <- list(score = score, at = at, frame = sys.nframe() + pos, storeData = storeData)
    else
      tmp_data <- list(score = thecall$score, at = thecall$at, frame = sys.nframe() + pos, storeData = storeData)
    object <- new(Class = "C1.curves", data = tmp_data, adeg.par = sortparameters$adepar, trellis.par = sortparameters$trellis, g.args = sortparameters$g.args, Call = match.call())
    
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
