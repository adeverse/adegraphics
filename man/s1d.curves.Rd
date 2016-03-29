\name{s1d.curves}
\alias{s1d.curves}

\title{1-D plot of multiple scores linked by curves}

\description{
  This function represents multiple scores using points linked by curves.
}

\usage{
s1d.curves(score, at = 1:NROW(score), facets = NULL, plot = TRUE, 
  storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{score}{a numeric matrix (or a data frame) used to produce the plot}
  
  \item{at}{a numeric vector used as an index}
  
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
  Graphical parameters for lines and points are available in \code{plines} and in \code{ppoints} of \code{adegpar}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}

\value{
  An object of class \code{ADEg} (subclass \code{C1.curves}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{C1.curves}}
  \code{\linkS4class{ADEg.C1}}
}

\examples{

scores <- matrix(1:50, nrow = 10)
s1d.curves(scores)
}

\keyword{aplot}
\keyword{hplot}

