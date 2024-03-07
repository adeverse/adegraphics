\name{triangle.traject}
\alias{triangle.traject}

\title{Ternary plot with trajectories}

\description{
This function represents a three dimensional scatter plot with trajectories.
}

\usage{
triangle.traject(dfxyz, fac = gl(1, nrow(dfxyz)), order, labels = levels(fac), 
  col = NULL, adjust = TRUE, min3d = NULL, max3d = NULL, showposition = TRUE, 
  facets = NULL, plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxyz}{a three columns data frame, the first system of coordinates, used to produce the plot}

  \item{fac}{a factor (or a matrix of factors) splitting the rows of \code{dfxyz}}
  
  \item{order}{a vector containing the drawing order of the trajectories. A vector of length equal to factor.}
  
  \item{labels}{a vector of character strings containing the trajectories' labels}
  
  \item{col}{a color or a colors vector to color points, labels and lines}

  \item{adjust}{a logical to adjust the device with the limits of the smaller equilateral triangle containing the values}
  
  \item{min3d}{a vector of three values for triangular minimal limits}
  
  \item{max3d}{a vector of three values for triangular maximal limits}
  
  \item{showposition}{a logical indicating whether the used triangle should be shown in the complete one}
  
  \item{facets}{a factor splitting the rows of \code{dfxyz} so that subsets
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
  The \code{fac} factor is used to display several trajectories: each level of \code{fac} is a specific trajectory. 
}

\value{
  An object of class \code{ADEg} (subclass \code{Tr.traject}) or \code{ADEgS} (if \code{showposition} is TRUE, 
  if \code{add} is \code{TRUE} and/or if facets are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{Tr.traject}}
  \code{\linkS4class{ADEg.Tr}}
}

\examples{
exo1 <- matrix(c(51.88, 32.55, 15.57, 44.94, 34.59, 20.47, 25.95, 39.15, 34.9,
  37.87, 43.19, 18.94, 34.2, 43.32, 22.48, 16.13, 42.18, 41.69,
  7.76, 70.93, 21.31, 6.22, 65.96, 27.82, 6.44, 57.06, 36.5,
  37.24, 32.45, 30.31, 16.09, 31.22, 52.69, 6.54, 24.68, 68.78), 
  ncol = 3, byr = TRUE)
exo1 <- as.data.frame(exo1)
names(exo1) <- c("agr", "ouv", "ter")
com <- as.factor(rep(c("Gig", "Lun", "Gan", "Mat"), c(3, 3, 3, 3)))
rec <- as.factor(rep(c("68", "75", "82"), 4))
row.names(exo1) <- paste(com, rec, sep = "")
tri1 <- triangle.traject(exo1, fac = com, showposition=FALSE, 
 pgrid.draw = FALSE, col = TRUE, axis.text = list(cex = 0))
}

\keyword{hplot}
\keyword{aplot}
