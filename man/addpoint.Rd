\name{addpoint}

\alias{addpoint}
\alias{addpoint-methods}
\alias{addpoint,ADEg-method}
\alias{addpoint,ADEgS-method}

\title{
  Adds points on graphics.
}

\description{
  Adds a \code{trellis} object containing one or several points on one or several graphical objects.
}

\usage{
addpoint(object, xcoord, ycoord, plot = TRUE, ...)
}

\arguments{
  \item{object}{an object of class \code{ADEg} or \code{ADEgS}
  }
  \item{xcoord}{an integer (or a vector) indicating where \code{label} is(are) plotted 
    on the x-axis, passed to the \code{panel.points} function of the \code{lattice} package
  }
  \item{ycoord}{an integer (or a vector) indicating where \code{label} is(are) plotted
    on the y-axis, passed to the \code{panel.points} function of the \code{lattice} package
  }
  \item{plot}{a logical indicating if the graphics is displayed
  }
  \item{\dots}{Other arguments. Additional graphical parameters (see the \code{ppoints} list in
    \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}}). 
    If \code{object} is an \code{ADEgS}, the argument \code{which} identify which 
    \code{ADEg} is/are used for superposition.
  }
}

\value{
   An object of class \code{"ADEgS"}.
}

\author{Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEgS}}
  \code{\link[lattice]{panel.points}}
}

\examples{
data(deug, package = "ade4")
deug$cent[1]
g1 <- s1d.density(deug$tab[, 1], plot = FALSE)
addpoint(g1, xcoord = deug$cent[1], ycoord = 0, ppoints = list(col = "black",
  pch = "*", cex = 3))
}

\keyword{aplot}
