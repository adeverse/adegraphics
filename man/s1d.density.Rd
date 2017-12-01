\name{s1d.density}
\alias{s1d.density}

\title{1-D plot of a numeric score by density curves}

\description{
  This function represents a score with a density curve for each level of a factor.
}

\usage{
s1d.density(score, fac = gl(1, NROW(score)), kernel = c("normal", "box", 
  "epanech", "biweight", "triweight"), bandwidth = NULL, gridsize = 450, 
  col = NULL, fill = TRUE, facets = NULL, plot = TRUE, storeData = TRUE,
  add = FALSE, pos = -1, ...)
}

\arguments{
  \item{score}{a numeric vector (or a data frame) used to produce the plot}
  
  \item{fac}{a factor (or a matrix of factors) to split \code{score}}
  
  \item{kernel}{the smoothing kernel used, see \code{\link[KernSmooth]{bkde}}}
  
  \item{bandwidth}{the kernel bandwidth smoothing parameter}
  
  \item{gridsize}{the number of equally spaced points at which to estimate the density}
  
  \item{col}{a logical, a color or a colors vector for labels, rugs, lines and polygons according to their factor level. 
    Colors are recycled whether there are not one color by factor level.}
    
  \item{fill}{a logical to yield the polygons density curves filled}
  
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
  \code{kernel}, \code{bandwidth} and \code{gridsize} are passed as parameters to \code{\link[KernSmooth]{bkde}} function of the \code{KernSmooth} package.
  
  Graphical parameters for rugs are available in \code{plines} of \code{adegpar} and the ones for density curves filled in \code{ppolygons}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}

\value{
  An object of class \code{ADEg} (subclass \code{C1.density}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or data frame for \code{score} or data frame for \code{fac} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{C1.density}}
  \code{\linkS4class{ADEg.C1}}
}

\examples{
score <- c(rnorm(1000, mean = -0.5, sd = 0.5), rnorm(1000, mean = 1))
fac <- rep(c("A", "B"), each = 1000)
s1d.density(score, fac, col = c(2, 4), p1d.reverse = TRUE)
}

\keyword{aplot}
\keyword{hplot}
