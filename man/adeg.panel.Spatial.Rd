\name{adeg.panel.Spatial}
\alias{adeg.panel.Spatial}

\title{Panel function for adding spatial objects.}

\description{
  Panel function adapted from the \code{Sp} package for displaying all kind of spatial
  objects handled by \code{Sp} (for classes inherited from the superclass \code{Spatial})
  into a \code{trellis} graphic (\code{lattice} package).
}

\usage{
adeg.panel.Spatial(SpObject, sp.layout = NULL, col = 1, border = 1, lwd = 1, 
  lty = 1, alpha = 0.8, cex = 1, pch = 20, n = length(col), spIndex = 1, ...)
}

\arguments{
  \item{SpObject}{an object of class \code{"SpatialPoints"}, \code{"SpatialPointsDataFrame"},\cr
    \code{"SpatialPixels"}, \code{"SpatialPixelsDataFrame"}, \code{"SpatialGrid"},\cr
    \code{"SpatialGridDataFrame"}, \code{"SpatialLines"}, \code{"SpatialLinesDataFrame"},\cr
    \code{"SpatialPolygons"} or \code{"SpatialPolygonsDataFrame"}}
    
  \item{sp.layout}{a list of layout items. See \code{spplot} for more information}
  
  \item{col}{background color (fill) of \code{Spobject}}
  
  \item{border}{border color}

  \item{lwd}{line width (border)}

  \item{lty}{line type (border)}

  \item{alpha}{background transparency of \code{Spobject}}
  
  \item{cex}{point size}
  
  \item{pch}{point type}

  \item{n}{if \code{SpObject} contains data, the  _desired_ number
    of intervals splitting the data (using \code{pretty}).}
    
  \item{spIndex}{if the \code{SpObject} contains a data frame, its values
    are represented with a color code. Only the \code{spIndex} data frame
    is represented}
    
  \item{\dots}{for coherence with panel functions} 
}

\value{
  Draws the Spatial object and layout.
}

\references{
  Package \code{Sp}. Author: Edzer Pebesma, Roger Bivand, Barry Rowlingson and Virgilo Gomez-Rubio.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\note{
  If \code{SpObject} contains several maps, only the first one is selected. 
  Also for objects containing more data (for classes \code{data.frame} with a slot \code{data}), this information is also shown. 
  To do so, various colors can be used (according to the col arguments).
  
  For more information about the use of panel functions, please see the \code{lattice} package developed by Deepayan Sarkar.
}

\examples{

if(require(lattice, quietly = TRUE) & require(sp, quietly = TRUE)) {
data(elec88, package = "ade4")

xy <- elec88$xy
arrow <- list("SpatialPolygonsRescale", offset = c(150000,1700000),
layout.north.arrow(), scale = 100000)

xyplot(xy[, 2] ~ xy[, 1], aspect = "iso", panel = function(...){
  adeg.panel.Spatial(SpObject = elec88$Spatial, sp.layout = list(arrow), 
  col = colorRampPalette(c("yellow", "blue"))(5), border =
  "transparent")})
}
}

\seealso{
  \code{\link[sp]{spplot}}
  \code{\link[sp]{sp.lines}}
  \code{\link[sp]{sp.polygons}}
  \code{\link[sp]{sp.grid}}
}

\keyword{aplot}
