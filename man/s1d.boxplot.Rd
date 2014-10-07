\name{s1d.boxplot}
\alias{s1d.boxplot}

\title{1-D box plot of a numeric score partitioned in classes (levels of a factor)}

\description{
  This function represents the link between a variable and a set of qualitative variables using box-and-whisker plots.
}

\usage{
s1d.boxplot(score, fac = gl(1, NROW(score)), at = 1:nlevels(fac), col = NULL, 
  facets = NULL, plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{score}{a numeric vector (or a data frame) used to produce the plot}
  
  \item{fac}{a factor (or a matrix of factors) to split \code{score}}
  
  \item{at}{a numeric vector used as an index}
  
  \item{col}{a color or a colors vector for points, labels, lines and polygons according to their factor level. 
    Colors are recycled whether there are not one color by factor level.}
  
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
  Graphical parameters for rugs are available in \code{plines} of \code{adegpar} and the ones for boxes in \code{ppolygons}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}

\value{
  An object of class \code{ADEg} (subclass \code{S1.boxplot}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or data frame for \code{score} or data frame for \code{fac} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S1.boxplot}}
  \code{\linkS4class{ADEg.S1}}
}

\examples{
data(banque, package = "ade4")
banque.acm <- ade4::dudi.acm(banque, scan = FALSE, nf = 4)
s1d.boxplot(banque.acm$l1[, 1], banque[, 2], psub.text = names(banque)[2], 
  psub.position = "topleft", col = c("red", "blue", "green", "purple", "orange"))
s1d.boxplot(banque.acm$l1[,1], banque[, 1:6], psub.position = "topleft")
}

\keyword{aplot}
\keyword{hplot}
