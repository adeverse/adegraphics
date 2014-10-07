\name{s1d.interval}
\alias{s1d.interval}

\title{1-D plot of the interval between two numeric scores}

\description{
  This function represents the interval between two scores using either segments or filled areas.
}

\usage{
s1d.interval(score1, score2, at = 1:NROW(score1), method = c("bars", "area"), 
  facets = NULL, plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{score1}{a numeric vector (or a data frame) used to produce the plot}
  
  \item{score2}{a numeric vector with as many values as values (or rows) in \code{score1}}
  
  \item{at}{a numeric vector used as an index}
  
  \item{method}{a value, \code{bars} or \code{area}, to represent either segments or areasbetween scores.}
  
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
  Graphical parameters for polygons, lines and segment boundaries are available in respectively 
  \code{ppolygons}, \code{plines} and \code{parrows} of \code{adegpar}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}

\value{
  An object of class \code{ADEg} (subclass \code{C1.interval}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or data frame for \code{score} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{C1.interval}}
  \code{\linkS4class{ADEg.C1}}
}

\examples{
set.seed(40)
sc1 <- rnorm(10)
sc2 <- rnorm(10)
s1d.interval(sc1, sc2, method = "bars")
s1d.interval(sc1, sc2, method = "area")
}

\keyword{aplot}
\keyword{hplot}

