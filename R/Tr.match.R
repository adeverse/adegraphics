setClass(
  Class = "Tr.match",
  contains = "ADEg.Tr",
)


setMethod(
  f = "initialize",
  signature = "Tr.match",
  definition = function(.Object, data = list(dfxyz = NULL, labels = NULL, frame = 0, storeData = TRUE), ...) {
    .Object <- callNextMethod(.Object, data = data, ...) ## ADEg.Tr initialize
    if(data$storeData)
      data$labels <- eval(data$labels, envir = sys.frame(data$frame))
    .Object@data$labels <- data$labels
    validObject(.Object)
    return(.Object)
  })


setMethod(
  f = "prepare",
  signature = "Tr.match",
  definition = function(object) {
    name_obj <- deparse(substitute(object))
    
    ## pre-management of graphics parameters      
    oldparamadeg <- adegpar()
    on.exit(adegpar(oldparamadeg))
    adegtot <- adegpar(object@adeg.par)
    
    ## object modification before calling inherited method
    object@adeg.par <- adegtot
    callNextMethod() ## prepare graph
    assign(name_obj, object, envir = parent.frame())
  })


setMethod(
  f = "Tr.panel",
  signature = "Tr.match",
  definition = function(object, x, y) {
    if(object@data$storeData) {
      labels <- object@data$labels
      df <- object@data$dfxyz
    } else {
      labels <- eval(object@data$labels, envir = sys.frame(object@data$frame))
      df <- eval(object@data$dfxyz, envir = sys.frame(object@data$frame))
    }
    
    if(NROW(df) %% 2)
      stop("error in Tr.panel method : unable to split the two datasets")
    
    df <- sweep(df, 1, rowSums(df), "/")
    
    mini3 <- object@g.args$min3d
    maxi3 <- object@g.args$max3d
        
    n <- NROW(df) / 2
    df1 <- df[1:n,]
    df2 <- df[(1 + n):(2 * n), ]
    mini3 <- object@g.args$min3d
    maxi3 <- object@g.args$max3d
    xyz1 <- .coordtotriangleM(df1, mini3 = mini3, maxi3 = maxi3)
    xyz2 <- .coordtotriangleM(df2, mini3 = mini3, maxi3 = maxi3)
    object@stats$coords2d1 <- xyz1[,2:3]
    object@stats$coords2d2 <- xyz2[,2:3]
    ## draw points
    if(any(object@adeg.par$ppoints$cex > 0))
      do.call("panel.points", c(list(x = xyz1[, 2], y = xyz1[, 3]), object@adeg.par$ppoints))

    ## draw arrows
    panel.arrows(x0 = xyz1[, 2], y0 = xyz1[, 3] , y1 = xyz2[, 3], x1 = xyz2[, 2],
                 angle = object@adeg.par$parrows$angle, length = object@adeg.par$parrows$length,
                 ends = object@adeg.par$parrows$end, lwd = object@adeg.par$plines$lwd, col = object@adeg.par$plines$col,
                 lty = object@adeg.par$plines$lty)
    
    if(any(object@adeg.par$plabels$cex > 0)) {
      xlab <- (xyz1[, 2] + xyz2[, 2]) / 2
      ylab <- (xyz1[, 3] + xyz2[, 3]) / 2
      object@adeg.par$plabels$optim <- FALSE
      adeg.panel.label(xlab, ylab, labels = labels, object@adeg.par$plabels)
    }
  })


triangle.match <- function(dfxyz1, dfxyz2, labels = row.names(as.data.frame(dfxyz1)), min3d = NULL, max3d = NULL, adjust = TRUE, 
  showposition = TRUE, facets = NULL, plot = TRUE, storeData = FALSE, add = FALSE, pos = -1, ...) {
                           
  ## evaluation of some parameters
  thecall <- .expand.call(match.call())
  data1 <- try(as.data.frame(eval(thecall$dfxyz1, envir = sys.frame(sys.nframe() + pos))), silent = TRUE)
  data2 <- try(as.data.frame(eval(thecall$dfxyz2, envir = sys.frame(sys.nframe() + pos))), silent = TRUE)
  
  if(class(data1) == "try-error" || class(data2) == "try-error" || is.null(thecall$dfxyz1) || is.null(thecall$dfxyz2))  ## wrong conversion 
    stop("non convenient selection for dfxyz1 or dfxyz2 (can not be converted to dataframe)")

  sortparameters <- .specificpar(...)

  ## facets
  if(!is.null(facets)) {
    object <- multi.facets.Tr(thecall, samelimits = sortparameters$g.args$samelimits)
  }
  
  ## simple ADEg graphic
  else {
    if(length(sortparameters$rest))
      warning(c("Unused parameters: ", paste(unique(names(sortparameters$rest)), " ", sep = "")), call. = FALSE)
    
    ## creation of the ADEg object
    g.args <- c(sortparameters$g.args, list(adjust = adjust, min3d = min3d, max3d = max3d))
    tmp_data <- list(dfxyz = call("rbind", thecall$dfxyz1, thecall$dfxyz2), labels = thecall$labels, frame = sys.nframe() + pos, storeData = storeData)
    object <- new(Class = "Tr.match", data = tmp_data, adeg.par = sortparameters$adepar, trellis.par = sortparameters$trellis, g.args = g.args, Call = match.call())
    
    ## preparation
    prepare(object)
    setlatticecall(object)
    if(showposition & add) {
      print("cannot show position and add") ## can be done, but modifies the meaning of the superposition
      showposition <- FALSE 
    }
    
    if(showposition)
      object <- new(Class = "ADEgS", ADEglist = list("triangle" = object, "positions" = .showpos(object)), positions = rbind(c(0, 0, 1, 1), c(0, 0.7, 0.3, 1)), add = matrix(0, ncol = 2, nrow = 2), Call = match.call())
    if(add)
      object <- add.ADEg(object)
  }
  
  if(!add & plot)
    print(object)
  invisible(object)  
}
