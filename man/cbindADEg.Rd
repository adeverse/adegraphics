\name{cbindADEg}

\alias{cbindADEg}
\alias{rbindADEg}
\alias{cbindADEg-methods}
\alias{rbindADEg-methods}
\alias{cbindADEg,ADEgORADEgSORtrellis,ADEgORADEgSORtrellis-method}
\alias{rbindADEg,ADEgORADEgSORtrellis,ADEgORADEgSORtrellis-method}

\title{Combine \code{ADEg} objects by columns or rows}

\description{
  Take a sequence of \code{ADEg}, \code{ADEgS} or \code{trellis} arguments and combine by columns or rows, respectively. 
}

\usage{
cbindADEg(g1, g2, ..., plot = FALSE)
rbindADEg(g1, g2, ..., plot = FALSE)
}

\arguments{
  \item{g1}{an object of class \code{ADEg}, \code{ADEgS} or \code{trellis}}
  \item{g2}{an object of class \code{ADEg}, \code{ADEgS} or \code{trellis}}
  \item{...}{other objects of class \code{ADEg}, \code{ADEgS} or \code{trellis}}
  \item{plot}{a logical indicating if the graphics is displayed}
}

\value{
  an \code{ADEgS} object
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEgS}}
  \code{\link{ADEgS}}
}

\examples{
data(jv73, package = "ade4")
pca1 <- ade4::dudi.pca(jv73$morpho, scannf = FALSE)
g1 <- s.label(pca1$li, plabels.optim = TRUE, plot = FALSE)
g2 <- s.class(pca1$li, jv73$fac.riv, starSize = 0, ellipseSize = 0, chullSize = 1, 
  ppolygons.alpha = 0.4, col = rainbow(12), ppoints.cex = 0, plot = FALSE)
g3 <- s.corcircle(pca1$co, pbackground.box = FALSE, plot = FALSE)
g4 <- rbindADEg(cbindADEg(g1, g2), cbindADEg(superpose(g1, g2), g3), plot = TRUE)
}

\keyword{hplot}
