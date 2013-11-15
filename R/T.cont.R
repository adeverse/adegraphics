setClass(
  Class = "T.cont",
  contains = "T.value"
)


setMethod(
  f = "T.panel",
  signature = "T.cont",
  definition = function(object, x, y) {
    ## call T.panel for T.value object
    callNextMethod(object, x, y)
    if(object@data$storeData) {
      dftab <- object@data$dftab
      x <- object@data$x
      y <- object@data$y
    } else {
      dftab <- eval(object@data$dftab, envir = sys.frame(object@data$frame))
      x <- eval(object@data$x, envir = sys.frame(object@data$frame))
      y <- eval(object@data$y, envir = sys.frame(object@data$frame))
    }
    
    dftab <- dftab / sum(dftab)
    
    f1 <- function(x, w) {
      w1 <- weighted.mean(w, x)
      w <- (w - w1)^2
      w2 <- sqrt(weighted.mean(w, x))
      return(c(w1, w2))
    }
    
    if(object@g.args$meanX) {
      val <- y
      w <- t(apply(dftab, 2, f1, w = val))
      panel.points(x = x, y = w[, 1], pch = 20, cex = 1.5, col = "black")
      panel.segments(x, w[, 1] - w[, 2] , x, w[, 1] + w[, 2], col = object@adeg.par$plines$col, lty = object@adeg.par$plines$lty, lwd = object@adeg.par$plines$lwd)
    }
    
    if(object@g.args$meanY) {
      w <- t(apply(dftab, 1, f1, w = x))
      panel.points(x = w[, 1], y, pch = 20, cex = 1.5, col = "black")
      panel.segments(w[, 1] - w[, 2], y, w[, 1] + w[, 2], y, col = object@adeg.par$plines$col, lty = object@adeg.par$plines$lty, lwd = object@adeg.par$plines$lwd)
    }
    
    x <- x[col(as.matrix(dftab))]
    y <- y[row(as.matrix(dftab))]
    if(object@g.args$ablineX)
      panel.abline(reg = lm(x ~ y, weights = as.vector(as.matrix(dftab))), col = object@adeg.par$plines$col, lty = object@adeg.par$plines$lty, lwd = object@adeg.par$plines$lwd)
    if(object@g.args$ablineY) {
      w <- coefficients(lm(x ~ y, weights = as.vector(as.matrix(dftab))))
      if(w[2] == 0)
        panel.abline(h = w[1], col = object@adeg.par$plines$col, lty = object@adeg.par$plines$lty, lwd = object@adeg.par$plines$lwd)
      else
        panel.abline(c(-w[1] / w[2], 1 / w[2]), col = object@adeg.par$plines$col, lty = object@adeg.par$plines$lty, lwd = object@adeg.par$plines$lwd)
    } 
  })
