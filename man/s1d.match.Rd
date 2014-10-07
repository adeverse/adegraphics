\name{s1d.match}
\alias{s1d.match}

\title{1-D plot of the matching between two numeric scores}

\description{
  This function represents paired scores with evenly spaced labels.
}

\usage{
s1d.match(score1, score2, labels = 1:NROW(score1), at = 0.5, 
  facets = NULL, plot = TRUE, storeData = TRUE, add = FALSE, 
  pos = -1, ...)
}

\arguments{
  \item{score1}{a numeric vector (or a data frame) used to produce the plot}
  
  \item{score2}{a numeric vector used to produce the plot with as many values as values (or rows) in \code{score1}}
  
  \item{labels}{the labels' names drawn for each \code{score1} value}
  
  \item{at}{a numeric vector used as an index}
  
  \item{facets}{a factor splitting \code{score1} so that subsets    % facets uniquement sur score1. quid de score2 
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
  Graphical parameters for rugs are available in \code{plines} of \code{adegpar}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}

\value{
  An object of class \code{ADEg} (subclass \code{S1.match}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or data frame for \code{score} or data frame for \code{fac} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S1.match}}
  \code{\linkS4class{ADEg.S1}}
}

\examples{
s1d.match(-5:5, 2 * (-5:5))
}

\keyword{aplot}
\keyword{hplot}
