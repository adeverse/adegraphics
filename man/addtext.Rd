\name{addtext}

\alias{addtext}
\alias{addtext-methods}
\alias{addtext,ADEgORtrellis-method}
\alias{addtext,ADEgS-method}

\title{
  Adds labels on graphics.
}

\description{
  Adds a \code{trellis} object containing one or several labels on one or several graphical objects.
}

\usage{
addtext(object, xcoord, ycoord, label, which = 1, plot = TRUE, pos = -1, ...)
}

\arguments{
  \item{object}{an object of class \code{ADEg}, \code{ADEgS} or \code{trellis}
  }
  \item{xcoord}{an integer (or a vector) indicating where \code{label} is(are) plotted on the x-axis
  }
  \item{ycoord}{an integer (or a vector) indicating where \code{label} is(are) plotted on the y-axis
  }
  \item{label}{a character string (or a vector) containing the label(s) displayed on \code{object}
  }
  \item{which}{if \code{object} is an \code{ADEgS}, which \code{ADEg} is used as the base of superposition 
  }
  \item{plot}{a logical indicating if the graphics is displayed
  }
  \item{pos}{an integer indicating the position of the
    environment where the data are stored, relative to the environment
    where the function is called. Useful only if \code{storeData} is
    \code{FALSE}
  }
  \item{\dots}{Additional graphical parameters (see the \code{plabels} list in
    \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}})
  }
}

\value{
   An object of class \code{"ADEgS"}.
}

\author{Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr}
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEgS}}
}

\examples{
data(dunedata, package = "ade4")
afc1 <- ade4::dudi.coa(dunedata$veg, scannf = FALSE)
g1 <- table.value(dunedata$veg, symbol = "circle", ppoints.cex = 0.5, plot = FALSE)
addtext(g1, 1, 20, "A", plabels.srt = 45)

xy <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -1, 1))
posi <- factor(xy$x > 0) : factor(xy$y > 0)
g2 <- s.class(xy, fac = posi, facets = posi, pellipses.col = 1:4, plabels.cex = 0, 
  plegend.drawKey = FALSE, psub.cex = 0, plot = FALSE)
addtext(g2, c(0.5, 0.5, -0.5, -0.5), c(0.5, -0.5), levels(posi), plabels.cex = 2, plabels.col = 1:4)
}

\keyword{aplot}
