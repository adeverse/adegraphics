s.hist <- function(dfxy, xax = 1, yax = 2, bandwidth, gridsize = 60, kernel = "normal", cbreaks = 2, storeData = FALSE, plot = TRUE, pos = -1, ...) {
  graphsnames <- c("labels", "densX", "densY", "link") 
  ## sorting parameters
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  ## setting positions
  positions <- layout2position(matrix(c(2, 4, 1, 3), 2, 2, byrow = TRUE), c(3, 1) / 2, c(1, 3) / 2, TRUE)
  ## first graphical object
  g1 <- do.call("s.label", args = c(list(substitute(dfxy), xax = xax, yax = yax, plot = FALSE, storeData = storeData, pos = (pos - 2)), sortparameters[[1]]))
  
  ## calculus needed
  xlimX <- g1@g.args$xlim
  ylimY <- g1@g.args$ylim
  breaks <- g1@s.misc$backgrid
  cgrid <- breaks$d / cbreaks
  bb1 <- range(breaks$x0[!is.na(breaks$x0)])
  bb2 <- range(breaks$y0[!is.na(breaks$y0)])
  breaksX <- seq(from = bb1[1], to = bb1[2], by = cgrid)
  breaksY <- seq(from = bb2[1], to = bb2[2], by = cgrid)
  while(min(breaksX) > xlimX[1])
    breaksX <- c((min(breaksX) - cgrid), breaksX)
  while(max(breaksX) < xlimX[2])
    breaksX <- c(breaksX, (max(breaksX) + cgrid))
  while(min(breaksY) > ylimY[1])
    breaksY <- c((min(breaksY) - cgrid), breaksY)
  while(max(breaksY) < ylimY[2])
    breaksY <- c(breaksY, (max(breaksY) + cgrid))
  
  ## limits and graduation
  limcalX <- hist(dfxy[, xax], breaksX, plot = FALSE)
  limcalY <- hist(dfxy[, yax], breaksY, plot = FALSE)
  top <- 1.1 * max(c(limcalX$counts, limcalY$counts))
  xlimY <- ylimX <- c(0, top)
  drawLines <- pretty(0:top)
  drawLines <- drawLines[-c(1, length(drawLines))]

  if(!missing(bandwidth)) {
    densiX <- bkde(dfxy[, xax], kernel = kernel, bandwidth = bandwidth, gridsize = gridsize)
    densiY <- bkde(dfxy[, yax], kernel = kernel, bandwidth = bandwidth, gridsize = gridsize)
  } else {
    densiX <- bkde(dfxy[, xax], kernel = kernel, gridsize = gridsize)
    densiY <- bkde(dfxy[, yax], kernel = kernel, gridsize = gridsize)
  }
  
  ## trellis creation 
  g2 <- xyplot(dfxy[, xax] ~ 1, xlim = xlimX, ylim = ylimX, horizontal = TRUE, scales = list(draw = FALSE), xlab = NULL, ylab = NULL, histValues = limcalX, drawLines = drawLines,  panel = function(x, y, histValues, horizontal, drawLines, ...) .adeg.panel.hist(y = y, histValues = histValues, horizontal = horizontal, drawLines = drawLines, densi = densiX, params = sortparameters[[2]], ...))
  g3 <- xyplot(dfxy[, yax] ~ 1, xlim = xlimY, ylim = ylimY, horizontal = FALSE, scales = list(draw = FALSE), xlab = NULL, ylab = NULL, histValues = limcalY, drawLines = drawLines, panel = function(x, y, histValues, horizontal, drawLines, ...) .adeg.panel.hist(y = y, histValues = histValues, densi = densiY, horizontal = horizontal, drawLines = drawLines, params = sortparameters[[3]], ...))
  g4 <- xyplot(1 ~ 1, xlim = xlimY, ylim = ylimX, scales = list(draw = F), xlab = NULL, ylab = NULL, drawLines = drawLines, panel = function(drawLines, ...) .adeg.panel.join(drawLines = drawLines, params = sortparameters[[4]], ...))

  ## ADEgS creation and display
  obj <- new(Class = "ADEgS", ADEglist = list(g1, g2, g3, g4), positions = positions, add = matrix(0, ncol = 4, nrow = 4), Call = match.call())
  names(obj) <- graphsnames
  if(plot)
    print(obj)
  invisible(obj)
}


## panel functions
.adeg.panel.hist <- function(y, horizontal = TRUE, histValues, densi, drawLines, params = list(), ..., identifier = "histogramADEg") {
  ## from panel.histogram of the lattice package
  plot.polygon <- modifyList(list(plot.polygon = trellis.par.get("plot.polygon")), params, keep.null = TRUE)[[1L]] ## hist params
  add.line <- modifyList(list(add.line = trellis.par.get("add.line")), params, keep.null = TRUE)[[1L]] ## backgroundlines
  plot.line <- modifyList(list(plot.line = trellis.par.get("plot.line")), params, keep.null = TRUE)[[1L]] ## density line
  
  h <- histValues
  a <- diff(range(y))   
  breaks <- h$breaks
  heiBar <- h$counts
  nb <- length(breaks)
  ## counts lines
  if(horizontal)
    do.call("panel.abline", c(list(h = drawLines), add.line))
  else
    do.call("panel.abline", c(list(v = drawLines), add.line))
  ## warning : density lines re-scale to check
  contdensi <- (h$counts[h$density != 0 & h$counts != 0] / h$density[h$density != 0 & h$counts != 0])[1]
  if(horizontal) {
    if(nb > 1) {
      panel.rect(x = h$mids, y = 0, height = heiBar, width = diff(breaks), 
                 col = plot.polygon$col, alpha = plot.polygon$alpha, border = plot.polygon$border, lty = plot.polygon$lty, 
                 lwd = plot.polygon$lwd, just = c("center", "bottom"), identifier = identifier)
    }
    do.call("panel.lines", c(list(x = densi$x, y = densi$y * contdensi), plot.line))
  } else {
    if(nb > 1)
      panel.rect(y = h$mids, x = 0, height = diff(breaks), width = heiBar, col = plot.polygon$col, alpha = plot.polygon$alpha, border = plot.polygon$border, lty = plot.polygon$lty, lwd = plot.polygon$lwd, just = c("left", "center"), identifier = identifier)
    do.call("panel.lines", c(list(y = densi$x,  x = densi$y * contdensi), plot.line))
  }
}


.adeg.panel.join <- function(drawLines, params, ...) {
  ## circle from c(0,0)p, radius = drawLines
  plot.line <- modifyList(list(add.line = trellis.par.get("add.line")), params, keep.null = TRUE)[[1L]] ## density line
  ## number of seg = 200
  plabels <- modifyList(adegpar("plabels"), params, keep.null = TRUE)[[1L]]
  scaleX <- c(current.panel.limits()$xlim, current.panel.limits()$ylim)
  xlines <- seq(from = min(scaleX) - 0.1 * min(scaleX), to = max(scaleX) * 1.1, length.out = 200)
  ylines <- lapply(drawLines, FUN = function(radius, x){
      indx <- (x <= radius) ## x can be greated than radius
      return(c(sqrt(radius * radius - x[indx] * x[indx]), (- sqrt(abs(radius * radius - x[!indx] * x[!indx])))))
  	}, x = xlines)

  trash <- lapply(ylines, FUN = function(y, x) {do.call("panel.lines", c(list(x = x[1:length(y)], y = y[1:length(y)]), plot.line))}, x = xlines)
  adeg.panel.label(x = sqrt(0.5) * drawLines, y = sqrt(0.5) * drawLines, as.character(drawLines), plabels)
}

