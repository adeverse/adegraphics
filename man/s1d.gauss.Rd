\name{s1d.gauss}
\alias{s1d.gauss}

\title{1-D plot of a numeric score by Gaussian curves}

\description{
  This function represents a score with a Gauss curve for each level of a factor.
}

\usage{
s1d.gauss(score, fac = gl(1, NROW(score)), wt = rep(1,
  NROW(score)), steps = 200, col = NULL, fill = TRUE,
  facets = NULL, plot = TRUE, storeData = TRUE, add =
  FALSE, pos = -1, ...)
}

\arguments{
  \item{score}{a numeric vector (or a data frame) used to produce the plot}
  
  \item{fac}{a factor (or a matrix of factors) to split \code{score}}
  
  \item{wt}{a vector of weights for \code{score}}
  
  \item{steps}{a value for the number of segments used to draw the Gauss curves}
  
  \item{col}{a logical, a color or a colors vector for labels, rugs, lines and polygons according to their factor level. 
    Colors are recycled whether there are not one color by factor level.}
    
  \item{fill}{a logical to yield the polygons Gauss curves filled}
  
  \item{facets}{a factor splitting \code{score} so that subsets
    of the data are represented on different sub-graphics}
    
  \item{plot}{a logical indicating if the graphics is displayed}
  
  \item{storeData}{a logical indicating if the data are stored in
    the returned object. If \code{FALSE}, only the names of the data
    arguments are stored}
    
  \item{add}{a logical. If \code{TRUE}, the graphic is superposed to the graphics
    already plotted in the current device}
    
  \item{pos}{an integer indicating the position of the
    environment where the data are stored, relative to the environment
    where the function is called. Useful only if \code{storeData} is
    \code{FALSE}}
    
  \item{\dots}{additional graphical parameters (see
    \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}})}
}

\details{
  Graphical parameters for rugs are available in \code{plines} of \code{adegpar} and the ones for Gauss curves filled in \code{ppolygons}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}

\value{
  An object of class \code{ADEg} (subclass \code{C1.gauss}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or data frame for \code{score} or data frame for \code{fac} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{C1.gauss}}
  \code{\linkS4class{ADEg.C1}}
}

\examples{
data(meau, package= "ade4")
envpca <- ade4::dudi.pca(meau$env, scannf = FALSE)
dffac <- cbind.data.frame(meau$design$season, meau$design$site)
g1 <- s1d.gauss(envpca$li[, 1], fac = dffac, fill = TRUE, col = 1:6)
update(g1, steps = 10)
g2 <- s1d.gauss(envpca$li[, 1], dffac[, 2], ppoly.col = 1:4, paxes.draw = TRUE, ylim = c(0, 2), 
  fill = TRUE, p1d.hori = FALSE)
}

\keyword{aplot}
\keyword{hplot}
