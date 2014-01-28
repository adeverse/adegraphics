## All inches!
getpositionleg <- function(labels, plegend, sizes, type) {
  ## this function computes and returns position and width/height for all legend elements (text, symbols, etc)
  n <- length(labels)
  
  wh <- function(value, axis = "x", n, valO = FALSE) {
    return(rep(convertUnit(value, unitTo = "inches", axisFrom = axis, axisTo = axis, typeFrom = "dimension", typeTo = "dimension", valueOnly = valO), length.out = n))}
  
  default.npc <- function(value) {
    if(is.unit(value))
      return(value)
    else return(unit(value, "npc"))
  }
  
  ## symbols
  heightsymbs <- rep(sizes, length.out = n) ## in inches
  widthsymbs <- rep(sizes, length.out = n) ## in inches
  
  ## text
  heighttexts <- stringHeight(labels) * plegend$text$cex
  widthtexts <- stringWidth(labels) * plegend$text$cex
  
  ## margin and space
  spaceX <- wh(default.npc(plegend$hspace), axis = "x", n = 1)  ## for width - in inches in dimension
  spaceY <- wh(default.npc(plegend$vspace), axis = "y", n = 1)  ## for height - in inches in dimension
  
  ## get legend dimension - in npc
  if(plegend$horizontal) {
  	hleg <- convertUnit(2 * spaceY + max(unit.c(heighttexts, unit(heightsymbs, "inches"))), axisTo = "y", axisFrom = "y", typeFrom = "dimension", typeTo = "dimension", unitTo = "npc", valueOnly = TRUE)
    wleg <- convertUnit(n * spaceX + sum(unit.c(widthtexts, unit(widthsymbs, "inches"))), axisFrom = "x", axisTo = "x", typeFrom = "dimension", typeTo = "dimension", unitTo = "npc", valueOnly = TRUE)
  } 
  else {
    maxhei <- unit(0, "npc")
    for(i in 1:n)
      maxhei <- unit.c(maxhei, max(heighttexts[i], unit(heightsymbs[i], "inches")))
    hleg <- convertUnit((n + 1) * spaceY + sum(maxhei), axisTo = "y", axisFrom = "y", typeFrom = "dimension", typeTo = "dimension", unitTo = "npc", valueOnly = TRUE)    
    wleg <- convertUnit(2 * spaceX + max(widthtexts) + max(unit(widthsymbs, "inches")), axisFrom = "x", axisTo = "x", typeFrom = "dimension", typeTo = "dimension", unitTo = "npc", valueOnly = TRUE)    
  }
  
  ## get legend position - in npc - align on the left of the legend on the x axis and on the bottom of the legend on the y axis
  positionLegend <- poslegend(plegend$position, type = type, w = wleg, h = hleg, horizontal = plegend$horizontal)
  
  ## convert text dimension in inches
  heighttexts <- wh(heighttexts, axis = "y", n, valO = TRUE)
  widthtexts <- wh(widthtexts, axis = "x", n, valO = TRUE)
  
  ## no unit anymore (all in inches)
  spaceX <- spaceX[[1]]
  spaceY <- spaceY[[1]]
  
  ## get positions of the first symbol of the legend (the leftmost and the lowermost)
  xsymb <- convertX(default.npc(positionLegend[1]), unitTo = "inches", valueOnly = TRUE) + widthsymbs[1] / 2 + spaceX
  ysymb <- convertY(default.npc(positionLegend[2]), unitTo = "inches", valueOnly = TRUE) + heightsymbs[1] / 2 + spaceY
  
  ## get positions of each text and each symbols - in inches
  if(plegend$horizontal) {
    xtext <- c()
    for(i in 2:n) {
      xtext <- c(xtext, xsymb[i - 1] + widthtexts[i - 1] + widthsymbs[i - 1] / 2 + spaceX / 2)
      xsymb <- c(xsymb, xtext[i - 1] + widthsymbs[i] / 2 + spaceX / 2)
    }
    xtext <- c(xtext, xsymb[n] + (widthsymbs[n]) / 2 + widthtexts[n])
    ysymb <- rep(ysymb, length.out = n)
    ytext <- ysymb
  } else {
    xsymb <- rep(xsymb, length.out = n)  ## same center (x) for every symbols
    xtext <- xsymb + (max(widthsymbs) + spaceX) / 2 + widthtexts
    for(i in 2:n)
      ysymb <- c(ysymb, ysymb[i - 1] + max(heightsymbs[2 - i + n], heighttexts[2 - i + n]) / 2 + max(heightsymbs[1 - i + n], heighttexts[1 - i + n]) / 2 + spaceY)
    ysymb <- rev(ysymb)
    ytext <- ysymb
  }
  
  return(list(xsymb = xsymb, ysymb = ysymb, maxSW = max(widthsymbs), maxSH = max(heightsymbs), xtext = xtext, ytext = ytext, maxTW = max(widthtexts), maxTH = max(heighttexts), spaceX = spaceX, spaceY = spaceY))
}



setvalueskey <- function(method, breaks, ppoints, plegend, symbol, center, type = c("S", "T")) {
  ## legend parameters: size, color, text
  maxsize <- max(abs(breaks))  ## the higher breaks is always consider as the reference
  l0 <- length(breaks)
  switch(method,
    size = {
      ## size proportional symbols
      if(nchar(center) < 5)
        breaks <- unique(c(breaks, signif(center, 5)))                                   
      breaks <- breaks[order(breaks, decreasing = FALSE)]
      l0 <- length(breaks)
      ## from original adeg
      breaks <- (breaks[1:(l0 - 1)] + breaks[2:l0]) / 2
      inter <- breaks
      sizes <- breaks - center
      ## size and color of symbols
      sizes <- .proportional_map(sizes, maxsize) * ppoints$cex[1]
      colorsymb <- ppoints$col[ifelse(breaks < center, 1, 2)]
      bordersymb <- ppoints$col[ifelse(breaks < center, 2, 1)]
    },
    
    color = {
      ## color symbols
      sizes <- rep(ppoints$cex[1], length.out = (l0 - 1))
      inter <- paste("[", breaks[l0], ";", breaks[l0 - 1], "]", sep = "")
      
      for(i in (l0 - 1):2)
        inter <- c(inter, paste("]", breaks[i], ";", breaks[i - 1], "]", sep = ""))

      colorsymb <- ppoints$col[1:length(inter)]
      bordersymb <- "black"
    })
  
  okleg <- list(sizes = sizes, col = colorsymb, border = bordersymb, textL = inter)
  n <- length(okleg$textL)
  
  cstnormal <- diff(current.panel.limits(unit = "inches")$xlim) / 10 ## scale / window panel size
  sizes <- okleg$sizes * cstnormal
  
  ## compute coordinates and width/height for legend
  posslegend <- getpositionleg(labels = okleg$textL, plegend = plegend, sizes = sizes, type = type)
  
  if(plegend$rect) {
    xleft <- min(posslegend$xsymb) - posslegend$maxSW / 2 - abs(posslegend$spaceX) / 2
    ybottom <- posslegend$ytext[n] - max(posslegend$maxTH, posslegend$maxSH) / 2 - abs(posslegend$spaceY) / 2
    xright <- max(posslegend$xtext) + abs(posslegend$spaceX)
    ytop <- posslegend$ytext[1] + max(posslegend$maxTH, posslegend$maxSH) / 2 + abs(posslegend$spaceY) / 2
    
    grid.rect(x = (xleft + xright) / 2,
      y = (ybottom + ytop) / 2,
      width = xright - xleft,
      height = ytop - ybottom,
      default.units = "inches",
      gp = gpar(col = plegend$border, fill = plegend$col, lwd = plegend$lwd, lty = plegend$lty, alpha = plegend$alpha)
    )
  }
  
  ## draw labels
  grid.text(label = okleg$textL, x = posslegend$xtext, y = posslegend$ytext, hjust = 1, default.units = "inches", gp = gpar(cex = plegend$text$cex, col = plegend$text$col))
  ## draw symbols  
  panel.symbols.grid(xx = posslegend$xsymb, yy = posslegend$ysymb, sizes = sizes, symbol = symbol, native = FALSE, col = okleg$col, border = okleg$border, alpha = ppoints$alpha) 
}


poslegend <- function(pos = "bottomleft", type = c("S", "T"), w, h, horizontal) {
  ## This functions determines the position of the legend (xy coordinates) from the 'pos' name
  ## type: S if for S2.*(inherited ADEg.S2 object)
  ## type: T if for T.* (inherited ADEg.T object)
  classpos <- match.arg(type)
  if(length(pos) == 2) {
    if(!is.unit(pos)) pos <- unit(pos, "npc")
    else pos <- c(convertUnit(pos[1], unitTo = "npc", axisFrom = "x"), convertUnit(pos[2], unitTo = "npc", axisFrom = "y"))
  }
  else if(is.character(pos)) {
    if(pos == "bottomleft")
      if(classpos == "S") pos <- unit(c(0, 0), "npc")
      else pos <- unit(c(0, -0.01 - h), "npc")
    else if(pos == "bottomright")
      if(classpos == "S") pos <- unit(c(1 - w, 0), "npc")   ## redo with the legend size
      else pos <- unit(c(1 - w, -0.01 - h), "npc")       
    else if(pos == "topleft")
      if(classpos == "S") pos <- unit(c(0, 1 - h), "npc")
      else pos <- unit(c(0 , 1.01), "npc")
    else if(pos == "topright")
      if(classpos == "S") pos <- unit(c(1 - w , 1 - h) , "npc")
      else pos <- unit(c(1 - w, 1.01), "npc")
  }
  return(pos)
}
