\name{s.match}
\alias{s.match}

\title{2-D scatter plot of the matching between two sets of coordinates}

\description{
  This function represents a two dimensional scatter plot linking paired coordinates.
}

\usage{
s.match(dfxy1, dfxy2, xax = 1, yax = 2, labels =
  row.names(as.data.frame(dfxy1)), arrows = TRUE, 
  facets = NULL, plot = TRUE, storeData = TRUE, 
  add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxy1}{a data frame, the first system of coordinates, used to produce the plot}

  \item{dfxy2}{a data frame, the second system of coordinates, with as many rows as \code{dfxy1}, used to produce the plot.}

  \item{labels}{a vector of character strings containing the matches' labels}
  
  \item{xax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the y-axis}

  \item{arrows}{a logical to draw arrows}
  
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
  An object of class \code{ADEg} (subclass \code{S2.match}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or vectors for \code{xax}/\code{yax} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.match}}
  \code{\linkS4class{ADEg.S2}}
}

\examples{
X <- data.frame(x = runif(50, -1, 2), y = runif(50, -1, 2))
Y <- X + rnorm(100, sd = 0.3)
g1 <- s.match(X, Y, arr = TRUE, ppoints.cex = 2, ppoints.col = c("blue", "green"))

data(doubs, package = "ade4")
dudi1 <- ade4::dudi.pca(doubs$env, scale = TRUE, scannf = FALSE, nf = 3)
dudi2 <- ade4::dudi.pca(doubs$fish, scale = FALSE, scannf = FALSE, nf = 2)
coin1 <- ade4::coinertia(dudi1, dudi2, scannf = FALSE, nf = 2)
g2 <- s.match(dfxy1 = coin1$mX, dfxy2 = coin1$mY)
}

\keyword{aplot}
\keyword{hplot}
