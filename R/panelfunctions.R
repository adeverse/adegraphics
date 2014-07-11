## Labels drawing
## TODO: labels' rotations.
## first, in no boxes, it is easy
## if boxes, at least do  90 degrees rotations
## finally, more than one rotation possible.
adeg.panel.label <- function(x, y, labels, plabels, pos = NULL) {
  if(any(plabels$cex > 0)) {
    n <- length(x)
    plboxes <- plabels$boxes  
    draw <- plabels$cex > 0
		## using .textsize funtion in utils.R
    textS <- .textsize(labels, plabels)
    srt <- textS$srt
    if(plboxes$draw && (srt != 0) & srt != 90) 
      warning("Boxes only implemented for 0 or 90 degrees rotation", call. = FALSE)
    ldraw <- rep(draw, length.out = n)  ## draw long enough
    ldraw[which(is.na(labels[1:n]) | labels[1:n] == "")] <- FALSE ## if no labels or null string don't bother
    width <- rep(textS$w, length.out = n)[ldraw]
    height <- rep(textS$h, lenght.out = n)[ldraw]
    lab <- rep(labels, length.out = n)[ldraw] ## no NA, removed using ldraw
    bdraw <- rep(plboxes$draw, length.out = length(ldraw))
    
    ## no boxes if no labels
    bdraw <- (bdraw & ldraw)
    ## labels a dessiner
    optim <- plabels$optim[1] ## only one possibility
    newpos <- list(x = x[ldraw], y = y[ldraw])
    
    if(optim) {                
      ## calcul des nouvelles positions uniquement pour les labels qui seront dessines
      ## informations sur panel
      nativelim <- current.panel.limits(unit = "native")
      incheslim <- current.panel.limits(unit = "inches")
      ## calcul des nouvelles positions.
      if(any(is.na(width)) | any(is.na(height)) | any(is.na(newpos$y)) | any(is.na(newpos$x))) 
        stop("NA restants revoir adeg.panel.label")
      newpos <- .pointLabel(x = newpos$x, y = newpos$y, labels = lab, width = width / diff(nativelim$xlim), height = height / diff(nativelim$ylim), limits = nativelim, xyAspect = diff(incheslim$xlim) / diff(incheslim$ylim), trace = FALSE)
    }
    
    if(any(bdraw)) {
      ## dessins de chaque boite avec son label
      plboxes <- lapply(plboxes, FUN = function(x) {rep(x, length.out = length(ldraw))})
      srt <- rep(srt, length.out = length(ldraw))
      plabels <- lapply(plabels, FUN = function(x) {rep(x, length.out = n)[ldraw]})
      for(i in 1:length(newpos$x)) {
        if(bdraw[i]) {
          ## labels sizes
          panel.rect(
            x = unit(newpos$x[i], "native"),
            y = unit(newpos$y[i], "native"),
            width = width[i],
            height = height[i],
            col = plboxes$col[i],
            alpha = plboxes$alpha[i],
            border = plboxes$border[i],
            lty = plboxes$lty[i],
            lwd = plboxes$lwd[i]
          )
        }
        panel.text(labels = lab[i], x = unit(newpos$x[i], "native"), y = unit(newpos$y[i], "native"), col = plabels$col[i], cex = plabels$cex[i], alpha = plabels$alpha[i], srt = srt[i])
      }
    }
    else { ## only text
      if(any(!ldraw)) ## obliger de repeter pour dessiner si un label doit etre ignorer
        panel.text(labels = lab, x = unit(newpos$x, "native"), y = unit(newpos$y, "native"),
          				 col = rep(plabels$col, length.out = length(ldraw))[ldraw], cex = rep(plabels$cex, length.out = length(ldraw))[ldraw], 
          				 alpha = rep(plabels$alpha, length.out = length(ldraw))[ldraw], rep(srt, length.out = length(ldraw))[ldraw], pos = pos)
      else
        panel.text(labels = lab, x = unit(newpos$x, "native"), y = unit(newpos$y, "native"), 
          				 col = plabels$col, cex = plabels$cex, alpha = plabels$alpha, srt = srt, pos = pos)
    }
  }
}


## symbol="circle" or "square"
## sizes = vecteur de taille (calculee par rapport a la surface d'un cercle
symbolschoice <- function(symbol, sizes) {
  switch(symbol,
         circle = {n <- 50
                   adj <- 0
                   sizes <- sizes
                  },
         square = {n <- 4
                   adj <- pi / 4
                   sizes <- sizes
                  })
  return(list(npolygon = n, adj = adj, sizes = sizes))
}


## draw symbol according information and for various sizes, used in panel.value graphics
## length(xx)==length(yy)==length(sizes)
## unit: inches mandatory
panel.symbols.grid <- function(xx, yy, sizes, symbol = "circle", ...) {
  
  adeggridpolygon <- function(x, y, size, col, border, alpha, symbol) {
    coord <- getpoly(n = whichis$npolygon, radius = size / 2, angle = whichis$adj) ## defaultwhichis$npolygon,radius=size/2,angle =whichis$adj)
    points <- list(x = x + coord[, 1], y = y + coord[, 2])
    do.call(grid.polygon, args = c(points, list(gp = gpar(fill = col, col = border, alpha = alpha), default.units = "inches"))) ## lpolygon slower 
  }
  
  dots <- list(...) ## recuperer info type couleur, border...
  ## choose symbol
  whichis <- symbolschoice(symbol, sizes) ## size ne sert plus a rien
  ## apply col, alpha and border
  selection <- lapply(dots[c("col", "border", "alpha")], FUN = function(x) {rep(x, length.out = length(xx))})
  ## show
  do.call(mapply, list(FUN = "adeggridpolygon", x = xx, y = yy, size = rep(sizes, length.out = length(xx)), col = selection$col, border = selection$border, alpha = selection$alpha, symbol = symbol))
}


## sizes will be given in "native" units
## a tester : la vitesse entre panel.symbols et panel.symbols.grid....
## a tester aussi: si bien pas de difference de taille
getpoly <- function(n = 4, radius = 1, angle = 0) {
  sequence <- seq(0, 2 * pi, length.out = n + 1) + angle
  return(cbind(cos(sequence), sin(sequence)) * radius)
}



#############################################
### Utiliser objet NB: neighbouring graph ###
### nb object: nb object, "liste"
### coord, coordonnees des points
### fonction tres inspire de plot.nb in spdep
adeg.panel.nb <- function(nbobject, coords, col.edge = "black", lwd = 1, lty = 1, pch = 20, cex = 1, col.node = "black", alpha = 1) {
  if(class(nbobject) != "nb")
    stop("nb object is not class nb") ## prevoir dans les fonctions user une selection de l element neighbourght si object de type listw
  if(length(nbobject) != nrow(coords))
    stop("error for nb object, not the same numbers of nodes and coordinates", call. = FALSE)
  edges <- cbind(rep(1:length(nbobject), lapply(nbobject, length)), unlist(nbobject))
  edges <- edges[edges[,2] != 0, ]
  
  ## ici faire rep des parametres pour pouvoir ensuite modifier couleur
  adeg.panel.edges(edges, coords, col.edge, lwd, lty, pch, cex, col.node, alpha)
}


## adeg.panel.edges....
## col, lwd, lty etc peuvent varier selon poids des connections
adeg.panel.edges <- function(edges, coords, col.edge = "black", lwd = 1, lty = 1, pch = 20, cex = 1, col.node = "black", alpha = 1) {
  panel.points(x = coords[, 1], y = coords[, 2], col = col.node, pch = pch, alpha = alpha, cex = cex)
  panel.segments(x0 = coords[edges[, 1], 1], y0 = coords[edges[, 1], 2], x1 = coords[edges[, 2], 1], y1 = coords[edges[, 2], 2], col = col.edge, lwd = lwd, lty = lty)
}


################## Panel.spatial #############################
## spObject can be :
## SpatialGridDataFrame","SpatialLinesDataFrame","SpatialPixelsDataFrame","SpatialPointsDataFrame","SpatialPolygonsDataFrame"
## n : nombre intervales si data 
## TODO: spObject pourrait etre une liste
adeg.panel.Spatial <- function(SpObject, sp.layout = NULL, col = 1, border = 1, lwd = 1, lty = 1, alpha = 0.8, cex = 1, pch = 20, n = length(col), spIndex = 1, ...) {

  if(length(grep("DataFrame", class(SpObject))) > 0) { ## there is data in 'SpObject' (it is a SpatialPolygonsDataFrame).
    mapSp <- try(SpObject[names(SpObject)[spIndex]], silent = TRUE) ## only the first map (spIndex = 1)
    values <- try(mapSp@data[, 1], silent = TRUE)
    
    if(is.factor(values)) { ## qualitative values
      if(length(col) != nlevels(values)) {
        if(length(col) == 1)  ## all values have the same color
          col <- rep(col, length.out = nlevels(values))
        else 
        	col <- adegpar()$ppalette$quali(nlevels(values))
      	colvalue <- col[values]
      } else
        colvalue <- col
    
    } else {  ## quantitative values
      breaks <- pretty(values, length(col))
      if((length(breaks) - 1) != length(col)) {
        if(length(col) == 1)  ## 'col' is not modified by the user
        	col <- adegpar()$ppalette$quanti(length(breaks) - 1)
        else  ## 'col' is modified but there is not enough color values
          col <- colorRampPalette(col)(length(breaks) - 1)
      }
      colvalue <- col[cut(values, breaks, include.lowest = TRUE)]
    }
  } else {  ## there is no data in 'SpObject'
    mapSp <- SpObject
    colvalue <- col
  }

   if(!is.null(sp.layout))
    sp.panel.layout(lst = sp.layout)
  
  if(inherits(SpObject, what = "SpatialPoints")) {
    ## insert ppoints.parameters for pch and cex
    sp.points(mapSp, col = colvalue, pch = pch, cex = cex, alpha = alpha)
  }
  
  if(inherits(SpObject, what = "SpatialPolygons"))
    sp.polygons(mapSp, col = border, fill = colvalue, alpha = alpha, lty = lty, lwd = lwd)
  
  ## For spatialLine problems ; no various colors
  if(inherits(SpObject, what = "SpatialLines"))
    sp.lines(mapSp, col = colvalue, alpha = alpha, lty = lty, lwd = lwd)
  if(inherits(SpObject, what = "SpatialGrid"))
    sp.grid(mapSp, at = breaks, col = col)
}


################ pour les value (s comme t) ##################
## x, y: coordonnees des symboles
## zcenter: z centre sur valeur center (par defaut 0): si method = color, changement de couleur,
## penser a resoudre 
adeg.panel.values <- function(x, y, z, method, symbol, ppoints, breaks, centerpar = NULL, center = 0) {
  if((length(x) != length(y)) | (length(y) != length(z)))
    stop("error in panel.values, not equal length for x, y, and z")
  
  maxsize <- max(abs(breaks))  ## toujours la meme reference, valeur max de division cad la valeur max que peut atteindre z
  zcenter <- z - center
  switch(method,
         size = { ## tailles proportionnelles aux valeurs de z (centre sur center)
           z <- zcenter[order(abs(zcenter), decreasing = TRUE)]
           x <- x[order(abs(zcenter), decreasing = TRUE)]
           y <- y[order(abs(zcenter), decreasing = TRUE)]
           if(!missing(center) & !is.null(centerpar)) {
             xnull <- x[z == 0]
             ynull <- y[z == 0]
           }
           sizes <- .proportional_map(z, maxsize) * ppoints$cex[1] ## une seule valeur prise en compte
           colorsymb <- sapply(z, FUN = function(x) {
             if(x < 0)
               return(ppoints$col[1])
             else
               return(ppoints$col[2])
           })
           borduresymb <- sapply(z, FUN = function(x) {
             if(x < 0)
               return(ppoints$col[2])
             else
               return(ppoints$col[1])})
         },
         color = { ## col va du plus fonce au plus clair (correspond des classe supe au inf)
           sizes <- ppoints$cex[1]
           breaks <- sort(breaks) ## breaks remis dans l'ordre croissant (z inf a sup)
           colorsymb <- ppoints$col[as.numeric(cut(zcenter, breaks, include.lowest = TRUE))]
           if(any(is.null(colorsymb)) | any(is.na(colorsymb)))
             stop("error while preparing color symbol", call. = FALSE)
           borduresymb <- "black"
         })
  cstnormal <- diff(current.panel.limits(unit = "inches")$xlim) / 10 ## normaliser/taille panel, cst 10 au hasard

  x <- convertUnit(unit(x, "native"), axisFrom = "x", axisTo = "x", typeFrom = "location", typeTo = "location", unitTo = "inches")
  y <- convertUnit(unit(y, "native"), axisFrom = "y", axisTo = "y", typeFrom = "location", typeTo = "location", unitTo = "inches")
  
  ## rm size always in inches
  panel.symbols.grid(xx = x, yy = y, sizes = sizes * cstnormal, symbol = symbol, col = rep(colorsymb, length.out = length(x))[1:length(x)], border = rep(borduresymb, length.out = length(x))[1:length(x)], alpha = ppoints$alpha)
  if(!missing(center) && !is.null(centerpar))
    panel.points(x = xnull, y = ynull, pch = centerpar$pch, col = centerpar$col, cex = centerpar$cex)
  return(cstnormal) ## renvoye a fonction panel si necessaire pour ajouter dans la legende. a stoker dans s.misc$maxvalue
}


## from http://rwiki.sciviews.org/doku.php?id=tips:graphics-grid:displaybitmap
## used in s.logo (rasterGrob) to handle pixmap objects

as.raster.pixmapRGB <- function(x, ...) {
  nr <- nrow(x@red)
  r <- rgb((x@red), (x@green), (x@blue))
  dim(r) <- x@size
  r
}


as.raster.pixmapGrey <- function(x, ...) {
  nr <- nrow(x@grey)
  r <- x@grey
  dim(r) <- x@size
  r
}
