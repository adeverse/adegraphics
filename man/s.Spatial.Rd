\name{s.Spatial}
\alias{s.Spatial}

\title{Mapping of a Spatial* object}

\description{This function represents a background map linked with data or not.
}

\usage{
s.Spatial(spObj, col = TRUE, nclass = 5, scale = TRUE, plot = TRUE, 
  storeData = TRUE, pos = -1, ...)
}

\arguments{
  \item{spObj}{an object deriving from class \code{Spatial} (package \code{sp})}
  
  \item{col}{a logical or a color to fill the background color of \code{spObj}}
  
  \item{nclass}{if \code{spObj} contains data, the desired number
    of intervals splitting the data (using \code{pretty})}
  
  \item{scale}{a \code{logical} indicating if numeric variables
    should be scaled}
    
  \item{plot}{a logical indicating if the graphics is displayed}
  
  \item{storeData}{a logical indicating if the data should be stored in
    the returned object. If \code{FALSE}, only the names of the data
    arguments are stored}
    
  \item{pos}{an integer indicating the position of the
    environment where the data are stored, relative to the environment
    where the function is called. Useful only if \code{storeData} is
    \code{FALSE}}
    
  \item{\dots}{additional graphical parameters (see
    \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}})}
}

\value{  
  An object of class \code{ADEg} (subclass \code{S2.label}) or \code{ADEgS} (if \code{spObj} contains more than one column ).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.label}}
  \code{\link[sp]{spplot}}
  \code{\link[sp]{sp.lines}}
  \code{\link[sp]{sp.polygons}}
  \code{\link[sp]{sp.grid}}
}


\examples{
data(elec88, package = "ade4")
## mapping whithout data
g1 <- s.Spatial(elec88$Spatial)

\dontrun{
  if(require(sp, quietly = TRUE)) {
    ## mapping whith data
    obj <- SpatialPolygonsDataFrame(Sr = elec88$Spatial, data = elec88$tab)
    g2 <- s.Spatial(obj)
    g3 <- s.Spatial(obj, nclass = 2, col = c("red", "blue"))
  }
}
}

\keyword{hplot}

