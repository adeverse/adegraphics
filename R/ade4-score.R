"score.acm" <- function (x, xax = 1, which.var = NULL, type = c("points", "boxplot"), pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "acm")) 
    stop("Object of class 'acm' expected")
  if(x$nf == 1) 
    xax <- 1
  if((xax < 1) || (xax > x$nf)) 
    stop("non convenient axe number")
  
  ## prepare
  oritab <- as.list(x$call)[[2]]
  evTab <- eval.parent(oritab)
  if(is.null(which.var))
    which.var <- 1:ncol(evTab)
  
  type <- match.arg(type)
  
  ## parameter management
  sortparameters <- sortparamADEg(...)
  params <- list()
  
  if(type == "boxplot") {
    ## parameter management
    params$adepar <- list(plabels = list(boxes = list(draw = FALSE)), p1d = list(rug = list(draw = TRUE)), 
                          paxes = list(draw = TRUE, y = list(draw = FALSE)), 
                          plegend = list(drawKey = FALSE), pgrid = list(text = list(cex = 0)),
                          psub = list(position = "topleft"))
    params$g.args <- list(samelimits = FALSE)
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    
    ## ADEgS creation
    ADEglist <- list()
    score <- x$l1[, xax]
    scorecall <- substitute(x$l1[, xax])
    
    for(i in which.var) {
      ## data management
      fac <- evTab[, i]
      faccall <- call("[", oritab, 1:NROW(evTab), i)
      
      ADEglist[[i]] <- do.call("s1d.boxplot", c(list(score = scorecall, fac = faccall, plot = FALSE, storeData = storeData, pos = pos - 2), 
                                                c(sortparameters$adepar, list(psub.text = paste0(colnames(evTab)[i], " (cr=", round(x$cr[i, xax], 2), ")"))),
                                                sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
    }
    ADEglist <- ADEglist[which.var]
    
    ## ADEgS creation
    posmatrix <- layout2position(.n2mfrow(length(which.var)), ng = length(which.var))
    object <- new(Class = "ADEgS", ADEglist = ADEglist, positions = posmatrix, add = matrix(0, ncol = length(which.var), nrow = length(which.var)), Call = match.call())
    
    
  } else if(type == "points") {
    ## parameter management
    params$adepar <- list(ppoints = list(pch = "|"), porigin = list(draw = FALSE), pgrid = list(draw = FALSE), 
                          psub = list(position = "topleft"), paxes = list(draw = TRUE), plabels = list(cex = 1.25))
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
    
    ## creation of each individual ADEg
    ADEglist <- list()
    score <- x$l1[, xax]
    scorecall <- substitute(x$l1[, xax])
    for(i in which.var) {
      ## data management
      fac <- evTab[, i]
      faccall <- call("[", oritab, 1:NROW(evTab), i)
      meangroup <- call("as.numeric", call("tapply", scorecall, faccall, mean))
      dfxy <- call("cbind", scorecall, call("as.numeric", call("[", meangroup, faccall)))
      
      ## ADEg creation
      g1 <- do.call("s.class", c(list(dfxy = dfxy, fac = faccall, ellipseSize = 0, plot = FALSE, storeData = storeData, pos = pos - 2), 
                                 c(sortparameters$adepar, list(psub.text = paste0(colnames(evTab)[i], " (cr=", round(x$cr[i, xax], 2), ")"))), 
                                 sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
      xlimg1 <- g1@g.args$xlim
      ylimg1 <- g1@g.args$ylim
      g2 <- xyplot(score ~ fac, xlab = "", ylab = "", scales = list(x = list(tck = c(1, 0)), y = list(tck = c(1, 0))), xlim = xlimg1, ylim = ylimg1, 
                   aspect = g1@adeg.par$paxes$aspectratio, panel = function(x, y) {panel.abline(h = as.numeric(tapply(y, x, mean)), a = 0, b = 1, lty = 1)})
      g2$call <- call("xyplot", substitute(scorecall ~ faccall), xlab = "", ylab = "", scales = list(x = list(tck = c(1, 0)), y = list(tck = c(1, 0))), xlim = substitute(xlimg1), ylim = substitute(ylimg1),
                      aspect = g1@adeg.par$paxes$aspectratio, panel = function(x, y) {panel.abline(h = as.numeric(tapply(y, x, mean)), a = 0, b = 1, lty = 1)})
      ADEglist[[i]] <- superpose(g2, g1, plot = FALSE)
    }
    ADEglist <- ADEglist[which.var]
    
    ## ADEgS creation
    posmatrix <- layout2position(.n2mfrow(length(which.var)), ng = length(which.var))
    object <- new(Class = "ADEgS", ADEglist = ADEglist, positions = posmatrix, add = matrix(0, ncol = length(which.var), nrow = length(which.var)), Call = match.call())
  } 
  
  names(object) <- colnames(evTab)[which.var]
  object@Call <- match.call()
  if(plot) 
    print(object)
  invisible(object)
}


"score.mix" <- function (x, xax = 1, which.var = NULL, type = c("points", "boxplot"), pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "mix")) 
    stop("Object of class 'mix' expected")
  if(x$nf == 1) 
    xax <- 1
  if((xax < 1) || (xax > x$nf)) 
    stop("non convenient axe number")
  
  ## internal function
  lm.pcaiv <- function(x, df, weights) {
    lm0 <- lm(as.formula(paste("reponse.generic ~ ", paste(names(df), collapse = "+"))), data = cbind.data.frame(x, df), weights = weights)
    return(predict(lm0))
  }
  
  ## data management
  oritab <- as.list(x$call)[[2]]
  evTab <- eval.parent(oritab)
  if(is.null(which.var)) 
    which.var <- 1:length(x$index)
  
  index <- as.character(x$index)
  score <- x$l1[, xax]
  scorecall <- substitute(x$l1[, xax])
  
  ADEglist <- list()
  for (i in which.var) {
    ## parameters management
    sortparameters <- sortparamADEg(...)
    params <- list()
    
    ## data management
    type.var <- index[i]
    col.var <- which(x$assign == i)
    y <- x$tab[, col.var]
    ycall <- substitute(x$tab[, col.var])
    
    ## type of variable : quantitative
    if(type.var == "q") {
      ## parameters management
      params$adepar <- list(psub = list(text = paste0(colnames(evTab)[i], " (r2=", round(x$cr[i, xax], 2), ")"), position = "topleft"), paxes = list(aspectratio = "fill", draw = TRUE), porigin = list(include = FALSE), pgrid = list(draw = FALSE), plabels = list(cex = 0))
      sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
      
      if(length(col.var) == 1) {
        g1 <- do.call("s.label", c(list(dfxy = call("cbind", scorecall, ycall), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
        g2 <- xyplot(y ~ score, panel = function(x, y) {panel.abline(lm(y ~ x), lty = 1)})
        g2$call <- call("xyplot", substitute(ycall ~ scorecall), panel = function(x, y) {panel.abline(lm(y ~ x), lty = 1)})
        ADEglist[[i]] <- superpose(g1, g2)
        
      } else {
        ## data management
        lm0 <- lm(as.formula(paste("reponse.generic ~ ", paste(names(y), collapse = "+"))), data = cbind.data.frame(reponse.generic = score, y), weights = rep(1, nrow(y))/nrow(y))
        lm0call <- substitute(lm(as.formula(paste("reponse.generic ~ ", paste(names(ycall), collapse = "+"))), data = cbind.data.frame(reponse.generic = scorecall, ycall), weights = rep(1, nrow(ycall))/nrow(ycall)))
        score.est <- predict(lm0)
        score.estcall <- substitute(predict(lm0call))
        ord0 <- order(y[, 1])
        ord0call <- substitute(order(ycall[, 1]))
        y1call <- call("[", ycall, ord0call, 1)
        x1call <- call("[", score.estcall, ord0call)
        
        ## ADEgS creation
        g1 <- do.call("s.label", c(list(dfxy = call("cbind", scorecall, call("[", ycall, 1:NROW(y), 1)), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
        g2 <- xyplot(y[ord0, 1] ~ score.est[ord0], panel = function(x, y) {panel.lines(x, y, lty = 1)})
        g2$call <- call("xyplot", substitute(y1call ~ x1call), panel = function(x, y) {panel.lines(x, y, lty = 1)})
        ADEglist[[i]] <- superpose(g1, g2)
      }
    }
    
    ## type of variable : factor
    else if(type.var == "f") {
      ## data management
      fac <- evTab[, i]
      faccall <- call("[", oritab, 1:NROW(evTab), i)
      meangroup <- call("as.numeric", call("tapply", scorecall, faccall, mean))
      dfxy <- call("cbind", scorecall, call("as.numeric", call("[", meangroup, faccall)))
      
      type <- match.arg(type)
      params <- list()
      
      if(type == "boxplot") {
        ## parameter management
        params$adepar <- list(plabels = list(boxes = list(draw = FALSE)), p1d = list(rug = list(draw = TRUE)), paxes = list(draw = TRUE, y = list(draw = FALSE)), 
                              plegend = list(drawKey = FALSE), pgrid = list(text = list(cex = 0)),
                              psub = list(text = paste0(colnames(evTab)[i], " (cr=", round(x$cr[i, xax], 2), ")"), position = "topleft"))
        params$g.args <- list(samelimits = FALSE)
        sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
        
        ## ADEgS creation
        ADEglist[[i]] <- do.call("s1d.boxplot", c(list(score = scorecall, fac = faccall, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
        
      } else if(type == "points") {
        ## parameter management
        params$adepar <- list(ppoints = list(pch = "|"), porigin = list(draw = FALSE), paxes = list(aspectratio = "fill", draw = TRUE), 
                              pgrid = list(draw = FALSE), 
                              psub = list(text = paste0(colnames(evTab)[i], " (cr=", round(x$cr[i, xax], 2), ")"), position = "topleft"))
        sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
        
        ## ADEg creation
        g1 <- do.call("s.class", c(list(dfxy = dfxy, fac = faccall, ellipseSize = 0, plot = FALSE, storeData = storeData, pos = pos - 2), 
                                   sortparameters$adepar, sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
        xlimg1 <- g1@g.args$xlim
        ylimg1 <- g1@g.args$ylim
        g2 <- xyplot(score ~ fac, xlab = "", ylab = "", xlim = xlimg1, ylim = ylimg1, 
                     aspect = g1@adeg.par$paxes$aspectratio, panel = function(x, y) {panel.abline(h = as.numeric(tapply(y, x, mean)), a = 0, b = 1, lty = 1)})
        g2$call <- call("xyplot", substitute(scorecall ~ faccall), xlab = "", ylab = "", xlim = substitute(xlimg1), ylim = substitute(ylimg1),
                        aspect = g1@adeg.par$paxes$aspectratio, panel = function(x, y) {panel.abline(h = as.numeric(tapply(y, x, mean)), a = 0, b = 1, lty = 1)})
        ADEglist[[i]] <- superpose(g2, g1, plot = FALSE)
      }
    }
    
    ## type of variable : ordered
    else if(type.var == "o") {
      ## parameters management
      params$adepar <- list(ppoints = list(pch = 20), paxes = list(aspectratio = "fill", draw = TRUE), 
                            porigin = list(draw = FALSE), pgrid = list(draw = FALSE), 
                            psub = list(text = paste0(colnames(evTab)[i], " (r2=", round(x$cr[i, xax], 2), ")"), position = "topleft"))
      sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
      
      ## data management
      lm0 <- lm(as.formula(paste("reponse.generic ~ ", paste(names(y), collapse = "+"))), data = cbind.data.frame(reponse.generic = score, y), weights = rep(1, nrow(y))/nrow(y))
      lm0call <- substitute(lm(as.formula(paste("reponse.generic ~ ", paste(names(ycall), collapse = "+"))), data = cbind.data.frame(reponse.generic = scorecall, ycall), weights = rep(1, nrow(ycall))/nrow(ycall)))
      score.est <- predict(lm0)
      score.estcall <- substitute(predict(lm0call))
      ord0 <- order(y[, 1])
      ord0call <- substitute(order(ycall[, 1]))
      y1call <- call("[", ycall, ord0call, 1)
      x1call <- call("[", score.estcall, ord0call)
      
      ## ADEgS creation
      g1 <- do.call("s.label", c(list(dfxy = call("cbind", scorecall, call("[", ycall, 1:NROW(y), 1)), plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$adepar, sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
      g2 <- xyplot(y[ord0, 1] ~ score.est[ord0], panel = function(x, y) {panel.lines(x, y)})
      g2$call <- call("xyplot", substitute(y1call ~ x1call), panel = function(x, y) {panel.lines(x, y)})
      ADEglist[[i]] <- superpose(g1, g2)
    }
  }
  ADEglist <- ADEglist[which.var]
  
  ## ADEgS creation
  posmatrix <- layout2position(.n2mfrow(length(which.var)), ng = length(which.var))
  object <- new(Class = "ADEgS", ADEglist = ADEglist, positions = posmatrix, add = matrix(0, ncol = length(which.var), nrow = length(which.var)), Call = match.call())
  names(object) <- colnames(evTab)[which.var]
  object@Call <- match.call()
  if(plot) 
    print(object)
  invisible(object)
}


"score.pca" <- function (x, xax = 1, which.var = NULL, pos = -1, storeData = TRUE, plot = TRUE, ...) {
  if(!inherits(x, "pca")) 
    stop("Object of class 'pca' expected")
  if(x$nf == 1) 
    xax <- 1
  if((xax < 1) || (xax > x$nf))
    stop("non convenient axe number")
  
  ## prepare
  oritab <- as.list(x$call)[[2]]
  type <- ade4::dudi.type(x$call)
  evTab <- eval.parent(oritab)
  if(is.null(which.var))
    which.var <- 1:ncol(evTab)
  
  ## parameter management
  sortparameters <- sortparamADEg(...)
  params <- list()
  params$adepar <- list(paxes = list(aspectratio = "fill", draw = TRUE), porigin = list(include = FALSE), pgrid = list(draw = FALSE), plabels = list(cex = 0))
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  ## creation of each individual ADEg
  ADEglist <- list()
  for(i in which.var) {
    typedudi <- if(type == 3) {paste0(" (r=", round(x$co[i, xax], 2), ")")} else {""}
    dfxy <- call("cbind", substitute(x$l1[, xax]), call("[", oritab, 1:NROW(evTab), i))
    
    g1 <- do.call("s.label", c(list(dfxy = dfxy, plot = FALSE, storeData = storeData, pos = pos - 2), 
                               c(sortparameters$adepar, list(psub.text = paste0(colnames(evTab)[i], typedudi))), 
                               sortparameters$trellis, sortparameters$g.args, sortparameters$rest))
    g2 <- xyplot(eval(dfxy)[, 2] ~ eval(dfxy)[, 1], aspect = g1@adeg.par$paxes$aspectratio, panel = function(x, y) {panel.abline(lm(y ~ x))})
    g2$call <- call("xyplot", substitute(dfxy[, 2] ~ dfxy[, 1]), aspect = g1@adeg.par$paxes$aspectratio, panel = function(x, y) {panel.abline(lm(y ~ x))})
    ADEglist[[i]] <- superpose(g1, g2)
  }
  ADEglist <- ADEglist[which.var]
  
  ## ADEgS creation
  posmatrix <- layout2position(.n2mfrow(length(which.var)), ng = length(which.var))
  object <- new(Class = "ADEgS", ADEglist = ADEglist, positions = posmatrix, add = matrix(0, ncol = length(which.var), nrow = length(which.var)), Call = match.call())
  names(object) <- colnames(evTab)[which.var]
  object@Call <- match.call()
  if(plot) 
    print(object)
  invisible(object)
}


"score.inertia" <- function(x, xax = 1, cont = 0.1, posieig = "none", pos = -1, storeData = TRUE, plot = TRUE, ...) { 
  
  if(!inherits(x, "inertia")) 
    stop("Object of class 'inertia' expected")
  
  ## data management
  ori <- as.list(x$call)
  evTab <- eval.parent(ori[[2]])
  
  if(length(xax) > 1)
    stop("Not implemented for multiple xax")
  if(xax > evTab$nf)
    stop("Non convenient xax")
  
  adegtot <- adegpar()
  position <- match.arg(posieig[1], choices = c("bottomleft", "bottomright", "topleft", "topright", "none"), several.ok = FALSE)

  ## sort parameters for each graph
  graphsnames <- c("light_row", "heavy_row", "light_col", "heavy_col", "eig")
  sortparameters <- sortparamADEgS(..., graphsnames = graphsnames)
  
  ## parameters management
  adegtot <- adegpar()
  params <- list()
  params$light_row <- list(plabels = list(cex = 0), ppoints = list(col = "grey20", alpha = 0.45, cex = 1.2, pch = 19))
  params$light_col <- list(plabels = list(cex = 0), ppoints = list(col = "grey20", alpha = 0.45, cex = 1.2, pch = 19))
  params$heavy_row <- list(plabels = list(boxes = list(draw = TRUE), col = "red", srt = "horizontal"), ppoints = list(col = "red", cex = 1.2, pch = 19))
  params$heavy_col <- list(plabels = list(boxes = list(draw = TRUE), col = "blue", srt = "horizontal"), ppoints = list(col = "blue", cex = 1.2, pch = 19))
  params$eig <- list(pbackground = list(box = TRUE), psub = list(text = "Eigenvalues"))
  sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  
  # never display points under contribution threshold
  sortparameters$light_row$plabels$cex <- 0
  sortparameters$light_col$plabels$cex <- 0
  
  ## management of the data and the parameters about the rows' contribution (individuals) on axes
  if(!is.null(x$row.rel)) {
    inertrow <- abs(x$row.rel[, xax]) / 100
    # inertrow <- sqrt(x$row.rel) / 100
    inertrowcall <- call("/", call("abs", call("[", call("$", substitute(x), "row.rel"), call(":", 1, call("NROW", call("$", substitute(x), "row.rel"))), xax)), 100)
    # inertrowcall <- call("/", call("sqrt", call("[", call("$", substitute(x), "row.rel"), call(":", 1, call("NROW", call("$", substitute(x), "row.rel"))), xax)), 100)
    lightrow <- subset(evTab$li[, xax], inertrow < cont)
    lightrowcall <- call("subset", call("[", call("$", ori[[2]], "li"), call(":", 1, call("NROW", call("$", ori[[2]], "li"))), xax), call("<", inertrowcall, cont))
    
    heavyrow <- subset(evTab$li[, xax], inertrow >= cont)
    heavyrowcall <- call("c", call("subset", call("[", call("$", ori[[2]], "li"), call(":", 1, call("NROW", call("$", ori[[2]], "li"))), xax), call(">=", inertrowcall, cont)), 0)
    if(length(heavyrow) == 0)
      stop("No points to draw, try lowering 'cont' (see 'x$row.rel')")
    heavy_inertrow <- subset(inertrow, inertrow >= cont)
    names_heavyrow <- subset(rownames(x$row.rel), inertrow >= cont)
    
    limglobal <- setlimits1D(mini = min(c(heavyrow, lightrow)), maxi = max(c(heavyrow, lightrow)), 
                             origin = adegtot$porigin$origin, includeOr = adegtot$porigin$include)
    params <- list()
    params$light_row <- list(xlim = limglobal)
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  }
  
  ## management of the data and the parameters about the columns' contribution (variables) on axes
  if(!is.null(x$col.rel)) {
    inertcol <- abs(x$col.rel[, xax]) / 100
    # inertcol <- sqrt(x$col.rel[, xax]) / 100
    inertcolcall <- call("/", call("abs", call("[", call("$", substitute(x), "col.rel"), call(":", 1, call("NROW", call("$", substitute(x), "col.rel"))), xax)), 100)
    # inertcolcall <- call("/", call("sqrt", call("[", call("$", substitute(x), "col.rel"), call(":", 1, call("NROW", call("$", substitute(x), "col.rel"))), xax)), 100)
    lightcol <- subset(evTab$co[, xax], inertcol < cont)
    lightcolcall <- call("subset", call("[", call("$", ori[[2]], "co"), call(":", 1, call("NROW", call("$", ori[[2]], "co"))), xax), call("<", inertcolcall, cont))
    
    heavycol <- subset(evTab$co[, xax], inertcol >= cont)
    heavycolcall <- call("c", call("subset", call("[", call("$", ori[[2]], "co"), call(":", 1, call("NROW", call("$", ori[[2]], "co"))), xax), call(">=", inertcolcall, cont)), 0)
    if(length(heavycol) == 0)
      stop("No points to draw, try lowering 'cont' (see 'x$col.rel')")
    heavy_inertcol <- subset(inertcol, inertcol >= cont)
    names_heavycol <- subset(rownames(x$col.rel), inertcol >= cont)
    
    limglobal <- setlimits1D(mini = min(c(heavycol, lightcol)), maxi = max(c(heavycol, lightcol)), 
                             origin = adegtot$porigin$origin, includeOr = adegtot$porigin$include)
    params <- list()
    params$light_col <- list(xlim = limglobal)
    sortparameters <- modifyList(params, sortparameters, keep.null = TRUE)
  }
  
  
  ## displaying of the eigen values
  if(position != "none")
    geig <- do.call("plotEig", c(list(eigvalue = call("$", ori[[2]], "eig"), nf = 1:evTab$nf, xax = xax, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$eig))
  
  ## function to create the graphics about the row' contribution (individuals) on axes
  f_row <- function(posi = "none", pos){
    graphnames <- c(if(length(lightrow) > 0) {"light_row"}, "heavy_row", "contribution", if(posi != "none") {"eig"})
    
    if(length(lightrow) > 0) {
      g1 <- do.call("s1d.label", c(list(score = lightrow, at = 0, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$light_row))
      g2 <- do.call("s1d.label", c(list(score = heavyrow, at = heavy_inertrow, labels = names_heavyrow, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$heavy_row))
      grow <- do.call("superpose", list(g1, g2))
      grow@Call <- call("superpose", list(g1@Call, g2@Call))
    } else {
      grow <- do.call("s1d.label", c(list(score = heavyrow, at = heavy_inertrow, labels = names_heavyrow, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$heavy_col))
    }
    # add an horizontal line drawinf the contribution threshold
    gcont <- xyplot(0 ~ 0, panel = function(x, y) {panel.abline(h = cont, lty = "dotted", col = "grey")})
    grow <- do.call("superpose", list(grow, gcont))
    grow@Call <- call("superpose", list(grow@Call, gcont$call))
    
    if(posi != "none")
      grow <- do.call("insert", list(geig, grow, posi = posi, plot = FALSE, ratio = 0.25))
    names(grow) <- graphnames
    return(grow)
  }
  
  # function to create the graphics about the columns' contribution (variables) on axes
  f_col <- function(posi = "none", pos) {
    graphnames <- c(if(length(lightcol) > 0) {"light_col"}, "heavy_col", "contribution", if(posi != "none") {"eig"})
    
    if(length(lightcol) > 0) {
      g3 <- do.call("s1d.label", c(list(score = lightcol, at = 0, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$light_col))
      g4 <- do.call("s1d.label", c(list(score = heavycol, at = heavy_inertcol, labels = names_heavycol, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$heavy_col))
      gcol <- do.call("superpose", list(g3, g4))
      gcol@Call <- call("superpose", list(g3@Call, g4@Call))
    } else {
      gcol <- do.call("s1d.label", c(list(score = heavycol, at = heavy_inertcol, labels = names_heavycol, plot = FALSE, storeData = storeData, pos = pos - 2), sortparameters$heavy_col))
    }
    # add an horizontal line drawinf the contribution threshold
    gcont <- xyplot(0 ~ 0, panel = function(x, y) {panel.abline(h = cont, lty = "dotted", col = "grey")})
    gcol <- do.call("superpose", list(gcol, gcont))
    gcol@Call <- call("superpose", list(gcol@Call, gcont$call))
    
    if(posi != "none")
      gcol <- do.call("insert", list(geig, gcol, posi = posi, plot = FALSE, ratio = 0.25))
    names(gcol) <- graphnames
    return(gcol)
  }
  
  ## function to create a layout of the graphics about the contribution of rows (individuals) and columns (variables) on axes
  f_both <- function(posi = "none", pos) {
    object <- do.call("cbindADEg", c(list(f_row(posi = "none", pos = pos - 1), f_col(posi = posi, pos = pos - 1))))
    names(object) <- c("row", "col")
    return(object)
  }
  
  ## creation of the appropriate plot according to the input data
  if(!is.null(x$row.rel) & is.null(x$col.rel))
    object <- f_row(posi = position, pos = pos)
  if(!is.null(x$col.rel) & is.null(x$row.rel))
    object <- f_col(posi = position, pos = pos)
  if(!is.null(x$row.rel) & !is.null(x$col.rel))
    object <- f_both(posi = position, pos = pos)
  if(is.null(x$row.rel) & is.null(x$col.rel))
    stop(paste("No inertia was calculated in the ", substitute(x), " object", sep = ""))
  
  object@Call <- match.call()
  
  if(plot)
    print(object)
  invisible(object)
}


#"score.coa" <- function (x, xax = 1, dotchart = FALSE, pos = -1, storeData = TRUE, plot = TRUE, ...) {
#
#  if(!inherits(x, "coa")) 
#    stop("Object of class 'coa' expected")
#  if(x$nf == 1) 
#    xax <- 1
#  if((xax < 1) || (xax > x$nf)) 
#    stop("non convenient axe number")
#  
#  if(dotchart)
#    stop("TRUE 'dotchart' not yet implemented")
#  
#  
#
#  def.par <- par(mar = par("mar"))
#  on.exit(par(def.par))
#  par(mar = c(0.1, 0.1, 0.1, 0.1))
#  
#  sco.distri.class.2g <- function(score, fac1, fac2, weight, labels1 = as.character(levels(fac1)), labels2 = as.character(levels(fac2)), clab1, clab2, cpoi, cet) {
#    nvar1 <- nlevels(fac1)
#    nvar2 <- nlevels(fac2)
#    ymin <- scoreutil.base(y = score, xlim = NULL, grid = TRUE, cgrid = 0.75, include.origin = TRUE, origin = 0, sub = NULL, csub = 0)
#    ymax <- par("usr")[4]
#    ylabel <- strheight("A", cex = par("cex") * max(1, clab1, clab2)) * 1.4
#    xmin <- par("usr")[1]
#    xmax <- par("usr")[2]
#    xaxp <- par("xaxp")
#    nline <- xaxp[3] + 1
#    v0 <- seq(xaxp[1], xaxp[2], le = nline)
#    
#    ## dessine la grille
#    segments(v0, rep(ymin, nline), v0, rep(ymax, nline), col = gray(0.5), lty = 1)
#    
#    ## dessine le cadre
#    rect(xmin, ymin, xmax, ymax)
#    
#    
#    sum.col1 <- unlist(tapply(weight, fac1, sum))
#    sum.col2 <- unlist(tapply(weight, fac2, sum))
#    sum.col1[sum.col1 == 0] <- 1
#    sum.col2[sum.col2 == 0] <- 1
#    
#    weight1 <- weight/sum.col1[fac1]
#    weight2 <- weight/sum.col2[fac2]
#    
#    y.distri1 <- tapply(score * weight1, fac1, sum)
#    y.distri1 <- rank(y.distri1)
#    y.distri2 <- tapply(score * weight2, fac2, sum)
#    y.distri2 <- rank(y.distri2) + nvar1 + 2
#    y.distri <- c(y.distri1, y.distri2)
#    
#    ylabel <- strheight("A", cex = par("cex") * max(1, clab1, clab2)) * 1.4
#    y.distri1 <- (y.distri1 - min(y.distri))/(max(y.distri) - min(y.distri))
#    y.distri1 <- ymin + ylabel + (ymax - ymin - 2 * ylabel) * y.distri1
#    y.distri2 <- (y.distri2 - min(y.distri))/(max(y.distri) - min(y.distri))
#    y.distri2 <- ymin + ylabel + (ymax - ymin - 2 * ylabel) * y.distri2
#    
#    for (i in 1:nvar1) {
#      w <- weight1[fac1 == levels(fac1)[i]]
#      y0 <- y.distri1[i]
#      score0 <- score[fac1 == levels(fac1)[i]]
#      x.moy <- sum(w * score0)
#      x.et <- sqrt(sum(w * (score0 - x.moy)^2))
#      x1 <- x.moy - cet * x.et
#      x2 <- x.moy + cet * x.et
#      etiagauche <- TRUE
#      if ((x1 - xmin) < (xmax - x2)) 
#        etiagauche <- FALSE
#      segments(x1, y0, x2, y0)
#      if (clab1 > 0) {
#        cha <- labels1[i]
#        cex0 <- par("cex") * clab1
#        xh <- strwidth(cha, cex = cex0)
#        xh <- xh + strwidth("x", cex = cex0)
#        yh <- strheight(cha, cex = cex0) * 5/6
#        if (etiagauche) 
#          x0 <- x1 - xh/2
#        else x0 <- x2 + xh/2
#        rect(x0 - xh/2, y0 - yh, x0 + xh/2, y0 + yh, col = "white", border = 1)
#        text(x0, y0, cha, cex = cex0)
#      }
#      points(x.moy, y0, pch = 20, cex = par("cex") * cpoi)
#    }
#    for (i in 1:nvar2) {
#      w <- weight2[fac2 == levels(fac2)[i]]
#      y0 <- y.distri2[i]
#      score0 <- score[fac2 == levels(fac2)[i]]
#      x.moy <- sum(w * score0)
#      x.et <- sqrt(sum(w * (score0 - x.moy)^2))
#      x1 <- x.moy - cet * x.et
#      x2 <- x.moy + cet * x.et
#      etiagauche <- TRUE
#      if ((x1 - xmin) < (xmax - x2)) 
#        etiagauche <- FALSE
#      segments(x1, y0, x2, y0)
#      if (clab2 > 0) {
#        cha <- labels2[i]
#        cex0 <- par("cex") * clab2
#        xh <- strwidth(cha, cex = cex0)
#        xh <- xh + strwidth("x", cex = cex0)
#        yh <- strheight(cha, cex = cex0) * 5/6
#        if (etiagauche) 
#          x0 <- x1 - xh/2
#        else x0 <- x2 + xh/2
#        rect(x0 - xh/2, y0 - yh, x0 + xh/2, y0 + yh, col = "white", border = 1)
#        text(x0, y0, cha, cex = cex0)
#      }
#      points(x.moy, y0, pch = 20, cex = par("cex") * cpoi)
#    }
#  }
#  
#  if (inherits(x, "witwit")) {
#    y <- eval.parent(as.list(x$call)[[2]])
#    oritab <- eval.parent(as.list(y$call)[[2]])
#  } else 
#    oritab <- eval.parent(as.list(x$call)[[2]])
#  
#  l.names <- row.names(oritab)
#  c.names <- names(oritab)
#  oritab <- as.matrix(oritab)
#  a <- x$co[col(oritab), xax]
#  a <- a + x$li[row(oritab), xax]
#  a <- a/sqrt(2 * x$eig[xax] * (1 + sqrt(x$eig[xax])))
#  a <- a[oritab > 0]
#  aco <- col(oritab)[oritab > 0]
#  aco <- factor(aco)
#  levels(aco) <- c.names
#  ali <- row(oritab)[oritab > 0]
#  ali <- factor(ali)
#  levels(ali) <- l.names
#  aw <- oritab[oritab > 0]/sum(oritab)
#  
#  sco.distri.class.2g(a, aco, ali, aw, clab1 = clab.c, clab2 = clab.r, cpoi = cpoi, cet = cet)
#  scatterutil.sub("Rows", csub = csub, possub = "topleft")
#  scatterutil.sub("Columns", csub = csub, possub = "bottomright")
#}