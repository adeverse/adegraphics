\name{triangle.match}
\alias{triangle.match}

\title{Ternary plot of the matching between two sets of coordinates}

\description{
This function represents a three dimensional scatter plot of paired coordinates.
}

\usage{
triangle.match(dfxyz1, dfxyz2, labels = row.names(as.data.frame(dfxyz1)), 
  min3d = NULL, max3d = NULL, adjust = TRUE, showposition = TRUE, facets = NULL, 
  plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxyz1}{a three columns data frame, the first system of coordinates, used to produce the plot}

  \item{dfxyz2}{a three columns data frame, the second system of coordinates, with as many rows as \code{dfxyz1}, used to produce the plot.}

  \item{labels}{a vector of character strings containing the matches' labels}

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
  An object of class \code{ADEg} (subclass \code{Tr.match}) or \code{ADEgS} (if \code{showposition} is TRUE, 
  if \code{add} is \code{TRUE} and/or if facets are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{Tr.match}}
  \code{\linkS4class{ADEg.Tr}}
}

\examples{
data(euro123, package = "ade4")
triangle.match(euro123$in78, euro123$in86, plabels.cex = 0.8)
}

\keyword{hplot}
\keyword{aplot}
