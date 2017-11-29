\name{addline}

\alias{addline}
\alias{addline-methods}
\alias{addline,ADEg-method}
\alias{addline,ADEgS-method}

\title{
  Adds lines on graphics.
}

\description{
  Adds a \code{trellis} object containing one or several lines on one or several graphical objects.
}

\usage{
addline(object, a = NULL, b = 0, h = NULL, v = NULL, plot = TRUE, ...)
}

\arguments{
  \item{object}{an object of class \code{ADEg} or \code{ADEgS}
  }
  \item{a, b}{coefficients of the line to be added, passed to the \code{panel.abline} 
    function of the \code{lattice} package
  }
  \item{h, v}{numeric vectors giving locations respectively of horizontal and 
    vertical lines to be added to the plot, in native coordinates, passed to the 
    \code{panel.abline} function of the \code{lattice} package
  }
  \item{plot}{a logical indicating if the graphics is displayed
  }
  \item{\dots}{Other arguments. Additional graphical parameters (see the \code{plines} list in
    \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}}). 
    If \code{object} is an \code{ADEgS}, the argument \code{which} identify which 
    \code{ADEg} is/are used for superposition.
  }
}

\value{
   An object of class \code{ADEgS}.
}

\author{Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEgS}}
  \code{\link[lattice]{panel.abline}}
}

\examples{
# example extracted from the pedagogic file, here: http://pbil.univ-lyon1.fr/R/pdf/tdr65.pdf
data(monde84, package = "ade4")
dfX <- cbind.data.frame(lpib = log(monde84$pib), croipop = monde84$croipop)
dfY <- cbind.data.frame(lmorta = log(monde84$morta), lanal = log(monde84$anal + 1), 
  rscol = sqrt(100 - monde84$scol))
dfX0 <- ade4::scalewt(dfX)
dfY0 <- ade4::scalewt(dfY)
can1 <- cancor(dfX0, dfY0)
varcanoX <- dfX0%*%can1$xcoef[,1]
varcanoY <- dfY0%*%can1$ycoef[,1]
g1 <- s.label(cbind(varcanoY,varcanoX), labels = row.names(monde84), plabel.cex = 0.8, plot = FALSE)
addline(g1, 0, 1, plines.col = "red", plines.lwd = 0.5, plines.lty = 2)
}

\keyword{aplot}
