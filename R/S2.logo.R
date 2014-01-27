#########################################################
###                       s.logo                      ##
#########################################################

## TODO: prendre en comptre differentes types de logo peut etre possible 
setClass(
  Class = "S2.logo",
  contains = "ADEg.S2",
)

setMethod(
  f = "initialize",
  signature = "S2.logo",
  definition = function(.Object, data = list(dfxy = NULL, logos = NULL, xax = 1, yax = 2, frame = 0, storeData = TRUE), ...) {
    .Object <- callNextMethod(.Object, data = data, ...) ## ADEg.S2 initialize
    if(data$storeData)
      data$logos <- eval(data$logos, envir = sys.frame(data$frame))
    .Object@data$logos <- data$logos
    return(.Object)
  })


setMethod(
  f = "prepare",
  signature = "S2.logo",
  definition = function(object) {
    name_obj <- deparse(substitute(object))
    
    ## pre-management of graphics parameters      
    oldparamadeg <- adegpar()
    on.exit(adegpar(oldparamadeg))
    adegtot <- adegpar(object@adeg.par)
    
    if(is.null(object@adeg.par$porigin$include) & (any(names(object@g.args) %in% c("Sp", "nbobject"))))
      adegtot$porigin$include <- FALSE
    
    ## object modification before calling inherited method
    object@adeg.par <- adegtot
    callNextMethod() ## prepare graph
    assign(name_obj, object, envir = parent.frame())
  })


setMethod(
  f = "S2.panel",
  signature = "S2.logo",
  definition = function(object, x, y) {
    ## list of pixmap object :logos
    if(object@data$storeData)
      logos <- object@data$logos
    else
      logos <- eval(object@data$logos, envir = sys.frame(object@data$frame))
    ## Here, x and y are converted in 'npc' units
    ## TODO: it would be better if we can use x and y in native coordinates, and then convert them when creating the viewport in pixmapGrob,
    
    ## drawing every logo
    ## attention ici on ne devrait pas avoir a convertir... pourquoi?
    
    xbis <- convertX(unit(x, "native"), unitTo = "npc", valueOnly = TRUE)
    ybis <- convertY(unit(y, "native"), unitTo = "npc", valueOnly = TRUE)
    for(i in 1:length(logos)) {
      grid.draw(pixmapGrob(logos[[i]], x = xbis[i], y = ybis[i], object@adeg.par$ppoints$cex, rect = object@g.args$rect))
    }
  })


## prototype, arguments to add according their use (ie: klogo?)
## to add: rectLogo
s.logo <- function(dfxy, logos, rect = TRUE, xax = 1, yax = 2, facets = NULL, plot = TRUE, storeData = FALSE, add = FALSE, pos = -1, ...) {
  
  ## evaluation of some parameters
  thecall <- .expand.call(match.call())
  df <- try(as.data.frame(eval(thecall$dfxy, envir = sys.frame(sys.nframe() + pos))), silent = TRUE)
  if((class(df) == "try-error") | is.null(thecall$dfxy)) ## non convenient dfxy argument
    stop("non convenient selection for dfxy (can not be converted to dataframe)")
  logos <- eval(thecall$logos, envir = sys.frame(sys.nframe() + pos))
  if(!is.list(logos))
    stop("The argument 'logos' should be a list")
  
  ## parameters sorted
  sortparameters <- .specificpar(...)
  
  ## facets
  if(!is.null(facets)) { 
    if((length(xax) == 1 & length(yax) == 1))
      object <- multi.facets.S2(thecall, sortparameters$adepar, samelimits = sortparameters$g.args$samelimits)
    else 
      stop("Facets are not allowed with multiple xax/yax")
  }
  
  ## multiple axes
  else if((length(xax) > 1 | length(yax) > 1)) {
    object <- multi.ax.S2(thecall)
  }
  
  ## simple ADEg graphic
  else {
    if(length(sortparameters$rest))
      warning(c("Unused parameters: ", paste(unique(names(sortparameters$rest)), " ", sep = "")), call. = FALSE)
    
    ## creation of the ADEg object
    g.args <- c(sortparameters$g.args, list(rect = thecall$rect))
    tmp_data <- list(dfxy = thecall$dfxy, xax = xax, yax = yax, logos = thecall$logos, frame = sys.nframe() + pos, storeData = storeData)
    object <- new(Class = "S2.logo", data = tmp_data, adeg.par = sortparameters$adepar, trellis.par = sortparameters$trellis, g.args = g.args, Call =  as.call(thecall))
    
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

