\name{s.corcircle}
\alias{s.corcircle}

\title{Correlation circle}

\description{
  This function produces a correlation circle.
}

\usage{
s.corcircle(dfxy, xax = 1, yax = 2, labels = row.names(as.data.frame(dfxy)), 
  fullcircle = TRUE, facets = NULL, plot = TRUE, storeData = TRUE, 
  add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxy}{a data frame used to produce the plot}
  
  \item{labels}{a vector containing the points' labels}
  
  \item{xax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the y-axis}

  \item{fullcircle}{a logical to include the complete circle (limits are then c(-1, 1))}
  
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
  An object of class \code{ADEg} (subclass \code{S2.corcircle}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or vectors for \code{xax}/\code{yax} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.corcircle}}
  \code{\linkS4class{ADEg.S2}}
}

\examples{
data (olympic, package = "ade4")
dudi1 <- ade4::dudi.pca(olympic$tab, scannf = FALSE)
g1 <- s.corcircle(dudi1$co)
g2 <- s.corcircle(dudi1$co, fullcircle = FALSE, pback.col = "grey")
}

\keyword{aplot}
\keyword{hplot}
