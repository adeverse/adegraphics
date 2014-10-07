\name{s.traject}
\alias{s.traject}

\title{2-D scatter plot with trajectories}

\description{
  This function represents a two dimensional scatter plot with trajectories.
}

\usage{
s.traject(dfxy, fac = gl(1, nrow(dfxy)), order, labels = levels(fac), 
  xax = 1, yax = 2, col = NULL, facets = NULL, plot = TRUE, 
  storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxy}{a data frame used to produce the plot}
  
  \item{fac}{a factor (or a matrix of factors) splitting the rows of \code{dfxy}}
  
  \item{order}{a vector containing the drawing order of the trajectories. A vector of length equal to factor.}
  
  \item{labels}{a vector of character strings containing the trajectories' labels}
  
  \item{xax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the y-axis}

  \item{col}{a color or a colors vector to color points, labels and lines}
  
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
  The \code{fac} factor is used to display several trajectories: each level of \code{fac} is a specific trajectory. 
}

\value{
  An object of class \code{ADEg} (subclass \code{S2.traject}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or multidimensional \code{fac} or vectors for \code{xax}/\code{yax} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.traject}}
  \code{\linkS4class{ADEg.S2}}
}

\examples{
rw <- function(a) {
  x <- 0
  for(i in 1:49) x <- c(x, x[length(x)] + runif(1, -1, 1))
  x
}
x1 <- unlist(lapply(1:5, rw))
y1 <- unlist(lapply(1:5, rw))
z1 <- gl(5, 50)
g1 <- s.traject(data.frame(x1, y1), z1, ppoints.pch = 19:23, plines.col = rainbow(5))

x2 <- unlist(lapply(1:2, rw))
y2 <- unlist(lapply(1:2, rw))
z2 <- gl(2, 50)
g2 <- s.traject(data.frame(x2, y2), z2, ppoints.pch = 21:20, plines.col = 1:2)
}

\keyword{hplot}
\keyword{aplot}
