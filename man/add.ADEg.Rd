\name{add.ADEg}

\alias{add.ADEg}
\alias{add.ADEg-methods}

\title{Superpose an new \code{ADEg} graph to the previous ones plotted}

\description{
  Adds an \code{ADEg} to the current \code{ADEg} or \code{ADEgS} plot. 
}

\usage{
add.ADEg(object)
}

\arguments{
  \item{object}{an \code{ADEg} object}
}

\details{
  This function uses the last plotted \code{ADEg} or \code{ADEgS} object.\cr
  It calls \code{\link{superpose}}.
}

\value{
  an \code{ADEgS} object
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link{superpose}}
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEgS}}
}

\examples{
df1 <- cbind(rnorm(24), rnorm(24))
df2 <- cbind(rnorm(24), rnorm(24))
g1 <- s.label(df1, ppoints.col = "blue")
g2 <- s.label(df2, ppoints.col = "red", plot = FALSE)
add.ADEg(g2)

data(jv73, package = "ade4")
pca1 <- ade4::dudi.pca(jv73$morpho, scannf = FALSE)
g5 <- s.label(pca1$li, plabels.optim = TRUE)
g6 <- s.class(pca1$li, jv73$fac.riv, starSize = 0, ellipseSize = 0, chullSize = 1, 
  ppolygons.alpha = 0.4, col = rainbow(12), ppoints.cex = 0, plot = FALSE)
add.ADEg(g6)
}

\keyword{aplot}
