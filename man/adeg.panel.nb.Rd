\name{adeg.panel.nb}
\alias{adeg.panel.nb}
\alias{adeg.panel.edges}

\title{Panel functions for adding graphs.}

\description{
  Panel function for representing a graph into a \code{trellis} graphic (\code{lattice} package).\cr
  Two types of graph objects can be used: \code{nb} or \code{listw} object (\code{spdep} package) or simple edges informations.\cr
  Directions associated with the edges are not displayed.\cr
}

\usage{
adeg.panel.nb(nbobject, coords, col.edge = "black", lwd = 1, lty = 1, pch = 20, 
  cex = 1, col.node = "black", alpha = 1)

adeg.panel.edges(edges, coords, col.edge = "black", lwd = 1, lty = 1, pch = 20, 
  cex = 1, col.node = "black", alpha = 1)
}

\arguments{
  \item{nbobject}{a object of class \code{nb} or \code{listw}}
  
  \item{edges}{a two columns matrix, representing the edges between the
    nodes. For a row i, x[i, 1] and x[i, 2] are linked, x[i, 1] and x[i, 2] being vertices number.}
    
  \item{coords}{a two columns matrix containing vertices' coordinates}
  
  \item{col.edge}{edges' color(s)}
  
  \item{lwd}{line width (edges). Can be a vector}
  
  \item{lty}{line type (edges). Can be a vector}
  
  \item{pch}{vertices' representation type (symbols). Can be a vector}
  
  \item{cex}{symbols' size(s) (vertices). Can be a vector}
  
  \item{col.node}{vertices' color(s). Can be a vector}
  
  \item{alpha}{symbols' transparency}
}

\value{
  Displays the neighboring graph.
}

\references{
  Package \code{spdep}. Author: Roger Bivand
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\note{
  For more information about the use of panel functions, please see the \code{lattice} package developed by Deepayan Sarkar.
}

\examples{
if(require(lattice, quietly = TRUE) &
  require(spdep, quietly = TRUE)) {

  data(elec88, package = "ade4")
  coords <- elec88$xy
  xyplot(coords[, 2] ~ coords[, 1],
    panel = function(...){adeg.panel.nb(elec88$nb, coords, col.edge = c("blue", "red"))})
}

if(require(lattice, quietly = TRUE)) {
  edges <- matrix(c(1, 2, 3, 2, 4, 1, 3, 4), byrow = TRUE, ncol = 2)
  coords <- matrix(c(0, 1, 1, 0, 0, -1, -1, 0), byrow = TRUE, ncol = 2)
  xyplot(coords[,2] ~ coords[,1],
    panel = function(...){adeg.panel.edges(edges, coords, lty = 1:4, cex = 5)})
}
}

\seealso{
  \code{\link[spdep]{plot.nb}}
}

\keyword{aplot}
