\name{s.density}
\alias{s.density}

\title{2-D scatter plot with kernel density estimation}

\description{
  This function represents a two dimensional scatter plot of points distribution.
  Densities' representation is based on the \code{levelplot} graphic in \code{lattice}
  (density's surface, filled with colors and/or contour lines).
}

\usage{
s.density(dfxy, xax = 1, yax = 2, bandwidth = NULL, gridsize = c(450L, 450L), 
  nrpoints = 300, threshold = 0.1, col = NULL, contour = FALSE, region = !contour, 
  nclass = 8, facets = NULL, plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxy}{a data frame used to produce the plot}
  
  \item{xax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the y-axis}

  \item{bandwidth}{bandwidth for density calculations which is passed in parameters in the \code{bkde2D} function of the \code{KernSmooth} package}
  
  \item{gridsize}{grid dimension}
    
  \item{nrpoints}{number of points on the density image}
  
  \item{threshold}{a value between 0 and 1 to draw densities greater than this threshold. No density is visible whether it is equal to 1}
    
  \item{col}{a color or a colors vector to color densities}
  
  \item{contour}{a logical to draw contour lines}
  
  \item{region}{a logical to fill grid regions with \code{col}}
  
  \item{nclass}{number of class for density}
  
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

\details{
  Density calculation is made using the \code{kde2d} function of the \code{KernSmooth} package.
}

\value{
  An object of class \code{ADEg} (subclass \code{S2.density}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or vectors for \code{xax}/\code{yax} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.density}}
  \code{\linkS4class{ADEg.S2}}
}
  
\examples{
xx2 <- c(rnorm(50000, 1, 1), rnorm(50000, -1, 1))
yy2 <- c(rnorm(50000, -1, 0.5), rnorm(50000, 1, 0.5))
s.density(cbind(xx2, yy2), paxes.draw = TRUE, gridsize = c(200, 200), region = TRUE, 
  contour = TRUE, plabels.cex = 0, threshold = 0.05, nclass = 3, 
  col = colorRampPalette(c("lightgrey", "black"))(100))
}

\keyword{hplot}
\keyword{aplot}
