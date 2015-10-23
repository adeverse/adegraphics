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

