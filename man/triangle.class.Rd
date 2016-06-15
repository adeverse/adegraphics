\name{triangle.class}
\alias{triangle.class}

\title{Ternary plot with a partition in classes (levels of a factor)}

\description{
  This function represents a three dimensional scatter plot with a partition in classes (levels of a factor).
}

\usage{
triangle.class(dfxyz, fac, wt = rep(1, NROW(fac)), labels = levels(fac), 
  col = NULL, ellipseSize = 1, starSize = 1, chullSize = NULL, adjust = TRUE, 
  min3d = NULL, max3d = NULL, showposition = TRUE, facets = NULL, plot = TRUE, 
  storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxyz}{a three columns data frame used to produce the plot}
  
  \item{fac}{a factor (or a matrix of factors) splitting the rows of \code{dfxyz}}
  
  \item{wt}{a vector of weights for \code{fac}}
  
  \item{labels}{a character vector containing the class' labels}
  
  \item{col}{a logical, a color or a colors vector to color points, ellipses, labels, lines and polygons}
  
  \item{ellipseSize}{a positive number for ellipse size}
  
  \item{starSize}{a number between 0 and 1 for the size of the stars segments joining the stars' center 
    (centroids) and the matching points}
    
  \item{chullSize}{\code{NULL} or a vector of numbers between 0 and 1 for the fraction of points included in the convex hull}
  
  \item{adjust}{a logical to adjust the device with the limits of the smaller equilateral triangle containing the values}
  
  \item{min3d}{a vector of three values for triangular minimal limits}
  
  \item{max3d}{a vector of three values for triangular maximal limits}
  
  \item{showposition}{a logical indicating whether the used triangle should be shown in the complete one}
  
  \item{facets}{a factor splitting the rows of \code{dfxyz} so that subsets
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

\value{
  An object of class \code{ADEg} (subclass \code{Tr.class}) or \code{ADEgS} (if \code{showposition} is TRUE, 
  if \code{add} is \code{TRUE} and/or if facets are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{Tr.class}}
  \code{\linkS4class{ADEg.Tr}}
}

\examples{
data(euro123, package = "ade4")
fac1 <- euro123$plan$an
df1 <- rbind.data.frame(euro123$in78, euro123$in86, euro123$in97)
triangle.class(df1, fac = fac1, showposition = TRUE, col = c(1, 2, 3))
triangle.class(df1, fac = fac1, showposition = FALSE, plabels.cex = 0, col = c(1, 2, 3), 
  key = list(space = "left"))
}

\keyword{hplot}
\keyword{aplot}
