\name{s1d.hist}
\alias{s1d.hist}

\title{1-D plot of a numeric score by bars}

\description{
  This function represents a score using a chart with rectangular bars.
}

\usage{
s1d.hist(score, breaks = NULL, nclass = round(log2(length(score)) + 1), 
  type = c("count", "density", "percent"), right = TRUE, facets = NULL, 
  plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{score}{a numeric vector (or a data frame) used to produce the plot}
  
  \item{breaks}{a vector of values to split \code{score}. If \code{NULL}, \code{pretty(score, nclass)} is used.}
  
  \item{nclass}{an integer for the number of desired intervals, ignored if \code{breaks} is not missing.}
  
  \item{type}{a value among \code{count}, \code{density}, \code{percent} to indicate the unit of the cell height.}
  
  \item{right}{a logical indicating if the histogram cells are right-closed (left open) intervals.}
  
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
  Graphical parameters for polygons are available in \code{ppolygons} of \code{adegpar}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}

\value{
  An object of class \code{ADEg} (subclass \code{C1.hist}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or data frame for \code{score} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{C1.hist}}
  \code{\linkS4class{ADEg.C1}}
  \code{\link[graphics]{hist}}
}

\examples{
set.seed(40)
score1 <- rnorm(1000)
s1d.hist(score1)
}

\keyword{aplot}
\keyword{hplot}

