\name{s.arrow}
\alias{s.arrow}

\title{2-D scatter plot with arrows}

\description{
  This function represents a two dimensional scatter plot with arrows linking points to the origin.
}

\usage{
s.arrow(dfxy, xax = 1, yax = 2, labels = row.names(as.data.frame(dfxy)), 
  facets = NULL, plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxy}{a data frame used to produce the plot}
    
  \item{xax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the y-axis}
  
  \item{labels}{a character vector containing labels for arrows}
  
  \item{facets}{a factor splitting the rows of \code{dfxy} so that subsets
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
  An other origin for arrows can be specified using an \code{adegpar} parameters: \code{porigin}.
  Graphical parameters for points and arrows are available in \code{parrows} and \code{ppoints} of \code{adegpar}.
}

\value{
  An object of class \code{ADEg} (subclass \code{S2.arrow}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or vectors for \code{xax}/\code{yax} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}


\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.arrow}}
  \code{\linkS4class{ADEg.S2}}
}

\examples{
data(doubs, package = "ade4") 
dudi1 <- ade4::dudi.pca(doubs$env, scale = TRUE, scannf = FALSE, nf = 3)
dudi2 <- ade4::dudi.pca(doubs$fish, scale = TRUE, scannf = FALSE, nf = 2)
coin1 <- ade4::coinertia(dudi1, dudi2, scannf = FALSE, nf = 2)
g11 <- s.arrow(coin1$l1, plabels.cex = 0.87, plot = FALSE)
g12 <- s.arrow(coin1$c1, plabels.cex = 1, plabels.col = "red", plot = FALSE)
g1 <- superpose(g12, g11, plot = TRUE)

xy <- cbind(rnorm(50), rnorm(50))
g2 <- s.arrow(xy, plabels.cex = 0.9, plines = list(lwd = 1.5), parrows.angle = 20)
update(g2, plines = list(col = rainbow(5)))
}

\keyword{aplot}
\keyword{hplot}
