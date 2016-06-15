\name{s.class}
\alias{s.class}

\title{2-D scatter plot with a partition in classes (levels of a factor)}

\description{
  This function represents a two dimensional scatter plot grouping points to the same class.
  Classes are represented by ellipses, stars and/or convex hulls.
}

\usage{
s.class(dfxy, fac, xax = 1, yax = 2, wt = rep(1, NROW(fac)), labels = levels(fac),
  ellipseSize = 1.5, starSize = 1, chullSize = NULL, col = NULL, facets = NULL,
  plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxy}{a data frame used to produce the plot}
  
  \item{fac}{a factor (or a matrix of factors) splitting the rows of \code{dfxy}}
  
  \item{xax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the y-axis}

  \item{wt}{a vector of weights for \code{fac}}
  
  \item{labels}{a character vector containing the class' labels}
  
  \item{ellipseSize}{a positive number for ellipse size}
  
  \item{starSize}{a number between 0 and 1 for the size of the stars segments joining the stars' center 
    (centroids) and the matching points}
    
  \item{chullSize}{\code{NULL} or a vector of numbers between 0 and 1 for the fraction of points included in the convex hull}
  
  \item{col}{a color or a colors vector to color points, ellipses, labels, lines and polygons}
  
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
  Graphical parameters for ellipses, stars and convex hulls are available in \code{pellipses}, \code{plines} and \code{ppolygons} of \code{adegpar}.
}

\value{
  An object of class \code{ADEg} (subclass \code{S2.class}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or multidimensional \code{fac} or vectors for \code{xax}/\code{yax} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.class}}
  \code{\linkS4class{ADEg.S2}}
}

\examples{
xy <- cbind.data.frame(x = runif(200, -1, 1), y = runif(200, -1, 1))
posi <- factor(xy$x > 0) : factor(xy$y > 0)
coul <- c("black", "red", "green", "blue")
s.class(xy, fac = posi, col = coul, psub.text = "example s.class", pellipses.col = coul)

s.class(xy, fac = posi, ppoints.cex = 1.5, ellipseSize = 0, starSize = 0,
  ppolygons = list(border = 4:1, col = 1:4, lty = 1:4, lwd = 2, alpha = 0.4),
  chullSize = c(1, 0.5))

s.class(xy, fac = posi, facets = posi, ppoints.cex = 1.5, ellipseSize = 0, starSize = 0,
  ppolygons = list(border = 4:1, col = 1:4, lty = 1:4, lwd = 2, alpha = 0.4),
  chullSize = c(1, 0.5))
  
\dontrun{ 
s.class(xy, fac = posi, col = coul, psub.text = "example s.class", pellipses.col = coul, 
  plabels.cex = 0, key = list(space = "left"))
   
data(banque, package = "ade4")
dudi1 <- ade4::dudi.acm(banque, scannf = FALSE)
col <- rainbow(length(levels(banque[, 20])))
g1 <- s.label(dudi1$li, psub = list(text = "Factorial map from ACM", cex = 1.5, 
  position = "topleft"), plot = FALSE)
g2 <- s.class(dudi1$li, banque[, 20], psub = list(text = names(banque)[20], cex = 1.5, 
  position = "bottomright"), ellipseSize = 0, starSize = 0.5, pgrid.text.cex = 0, plot = FALSE)
g3 <- s.class(dudi1$li, banque[, 20], starSize = 0, ellipseSize = 2, pgrid.text.cex = 0, 
  plabels.cex = 1.5, plot = FALSE)
g4 <- s.class(dudi1$li, banque[, 20], psub = list(text = names(banque)[20], 
  position = "topright"), pgrid.text.cex = 0, col = col, pellipses.lwd = 1.5, plot = FALSE)
G1 <- ADEgS(c(g1, g2, g3, g4), layout = c(2, 2))
G2 <- s.class(dudi1$li, banque, psub = list(position = "topleft"), pgrid.text.cex = 0, 
  starSize = 0, ppoints.cex = 0)
}
}

\keyword{aplot}
\keyword{hplot}
