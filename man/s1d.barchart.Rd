\name{s1d.barchart}
\alias{s1d.barchart}

\title{1-D plot of a numeric score by bars}

\description{
  This function represents a score using a chart with rectangular bars for which length is proportional to this score.
}

\usage{
s1d.barchart(score, labels = NULL, at = 1:NROW(score), facets = NULL, 
  plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{score}{a numeric vector (or a data frame) used to produce the plot}
  
  \item{labels}{the labels' names drawn on the top of bars}
  
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
  Graphical parameters for bars are available in \code{ppolygons} of \code{adegpar}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}

\value{
  An object of class \code{ADEg} (subclass \code{C1.barchart}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or data frame for \code{score} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{C1.barchart}}
  \code{\linkS4class{ADEg.C1}}
}

\examples{
data(rpjdl, package = "ade4")
rpjdl.coa <- ade4::dudi.coa(rpjdl$fau, scannf = FALSE, nf = 4)
s1d.barchart(rpjdl.coa$eig, p1d.horizontal = FALSE, ppolygons.col = "grey")
}

\keyword{aplot}
\keyword{hplot}

