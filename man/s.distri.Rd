\name{s.distri}
\alias{s.distri}

\title{2-D scatter plot with means/standard deviations computed using an external table of weights}

\description{
  This function represents a two dimensional scatter plot of a frequency distribution.
  Class are defined by ellipses and/or stars.
}

\usage{
s.distri(dfxy, dfdistri, xax = 1, yax = 2, starSize = 1, 
  ellipseSize = 1.5, col = NULL, facets = NULL, plot = TRUE, 
  storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxy}{a data frame used to produce the plot}
  
  \item{dfdistri}{a data frame containing the mass distribution in columns}
    
  \item{xax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the y-axis}

  \item{starSize}{\code{NULL} or number between 0 and 1 for the size of the stars segments joining the stars' center 
    (centroids) and the matching points}

  \item{ellipseSize}{\code{NULL} or number between 0 and 1 for ellipse size}

  \item{col}{a color or a colors vector to color points, ellipses, labels, lines and polygons}

  \item{facets}{a factor splitting the rows of \code{dfxy} so that subsets
    of the data are represented on different sub-graphics}
    
  \item{plot}{a logical indicating if the graphics is displayed}
  
  \item{storeData}{a logical indicating if the data should be stored in
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

\value{
  An object of class \code{ADEg} (subclass \code{S2.distri}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or vectors for \code{xax}/\code{yax} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.distri}}
  \code{\linkS4class{ADEg.S2}}
}

\examples{
data(rpjdl, package = "ade4")
xy <- ade4::dudi.coa(rpjdl$fau, scan = FALSE)$li
j <- c(1, 5, 8, 20, 21, 23, 26, 33, 36, 44, 47, 49)
dfdistri <- rpjdl$fau[, j]
coli <- colorRampPalette(c("blue", "red", "orange"))(49)[j]

s.distri(xy, dfdistri, ellipseSize = 1, starSize = 0, porigin.include = FALSE, 
  pellipses = list(col = coli, alpha = 0.3), plabels.cex = 0)
}

\keyword{aplot}
\keyword{hplot}
