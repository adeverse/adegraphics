\name{s1d.distri}
\alias{s1d.distri}

\title{1-D plot of a numeric score by means/standard deviations computed
using an external table of weights}

\description{
  This function represents a set of distributions on a numeric score using a mean-standard deviation display
}

\usage{
s1d.distri(score, dfdistri, labels = colnames(dfdistri), at = 1:NCOL(dfdistri), 
  yrank = TRUE, sdSize = 1, facets = NULL, plot = TRUE, 
  storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{score}{a numeric vector (or a data frame) used to produce the plot}
  
  \item{dfdistri}{a data frame containing the mass distribution in which each column is a class}
  
  \item{yrank}{a logical to draw the distributions sorted by means ascending order}
  
  \item{labels}{the labels' names drawn for each distribution}
  
  \item{at}{a numeric vector used as an index}
  
  \item{sdSize}{a numeric for the size of the standard deviation segments}
  
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
  Graphical parameters for rugs are available in \code{plines} of \code{adegpar}.
  Some appropriated graphical parameters in \code{p1d} are also available.
  The weighted means and standard deviations of class are available in the object slot \code{stats} using \code{object@stats$means} and \code{object@stats$sds}.
}

\value{
  An object of class \code{ADEg} (subclass \code{S1.distri}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or data frame for \code{score} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S1.distri}}
  \code{\linkS4class{ADEg.S1}}
}

\examples{
w <- seq(-1, 1, le = 200)
distri <- data.frame(lapply(1:50, 
  function(x) sample(200:1) * ((w >= (- x / 50)) & (w <= x / 50))))
names(distri) <- paste("w", 1:50, sep = "")
g11 <- s1d.distri(w, distri, yrank = TRUE, sdS = 1.5, plot = FALSE)
g12 <- s1d.distri(w, distri, yrank = FALSE, sdS = 1.5, plot = FALSE)
G1 <- ADEgS(c(g11, g12), layout = c(1, 2))

data(rpjdl, package = "ade4")
coa1 <- ade4::dudi.coa(rpjdl$fau, scannf = FALSE)
G2 <- s1d.distri(coa1$li[,1], rpjdl$fau, labels = rpjdl$frlab, 
  plabels = list(cex = 0.8, boxes = list(draw = FALSE)))

\dontrun{
g31 <- s1d.distri(coa1$l1[,1], rpjdl$fau, plabels = list(cex = 0.8, boxes = list(draw = FALSE)), 
  plot = FALSE)
nsc1 <- ade4::dudi.nsc(rpjdl$fau, scannf = FALSE)
g32 <- s1d.distri(nsc1$l1[,1], rpjdl$fau, plabels = list(cex = 0.8, boxes = list(draw = FALSE)), 
  plot = FALSE)
g33 <- s.label(coa1$l1, plot = FALSE)
g34 <- s.label(nsc1$l1, plot = FALSE)
G3 <- ADEgS(c(g31, g32, g33, g34), layout = c(2, 2))
} 
}

\keyword{aplot}
\keyword{hplot}
