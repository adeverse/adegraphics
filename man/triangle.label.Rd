\name{triangle.label}
\alias{triangle.label}

\title{Ternary plot with labels}

\description{
  This function represents a three dimensional scatter plot with labels.
}

\usage{
triangle.label(dfxyz, labels = rownames(dfxyz), adjust = TRUE, min3d = NULL, 
  max3d = NULL, addaxes = FALSE, addmean = FALSE, meanpar = NULL, axespar = NULL, 
  showposition = TRUE, facets = NULL, plot = TRUE, storeData = TRUE, add = FALSE, 
  pos = -1, ...)
}

\arguments{
  \item{dfxyz}{a three columns data frame used to produce the plot}
  
  \item{labels}{a character vector containing labels for points}
  
  \item{adjust}{a logical to adjust the device with the limits of the smaller equilateral triangle containing the values}
  
  \item{min3d}{a vector of three values for triangular minimal limits}
  
  \item{max3d}{a vector of three values for triangular maximal limits}
  
  \item{addaxes}{a logical to draw the principal axes}
  
  \item{addmean}{a logical to plot the mean}
  
  \item{meanpar}{a list to represent mean points using \code{pch}, \code{cex} and \code{col}}
  
  \item{axespar}{a list to represent axes lines using \code{col}, \code{lwd} and \code{lty}}
  
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

\value{
  An object of class \code{ADEg} (subclass \code{Tr.label}) or \code{ADEgS} (if \code{showposition} is TRUE, 
  if \code{add} is \code{TRUE} and/or if facets are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{Tr.label}}
  \code{\linkS4class{ADEg.Tr}}
}

\examples{
data(euro123, package = "ade4")
df <- rbind.data.frame(euro123$in78, euro123$in86, euro123$in97)
row.names(df) <- paste(row.names(euro123$in78), rep(c(1, 2, 3), rep(12, 3)), sep = "")
g1 <- triangle.label(df, label = row.names(df), showposition = TRUE, plot = FALSE)
g2 <- triangle.label(euro123$in78, plabels.cex = 0, ppoints.cex = 2, addmean = TRUE, 
  show = FALSE, plot = FALSE)
g3 <- triangle.label(euro123$in86, labels = row.names(euro123$in78), plabels.cex = 0.8, 
  plot = FALSE)
g4 <- triangle.label(rbind.data.frame(euro123$in78, euro123$in86), plabels.cex = 0.8, 
  addaxes = TRUE, psub.te = "Principal axis", psub.cex = 1.5, psub.pos = "topright", plot = FALSE)
G <- ADEgS(c(g1, g2, g3, g4), layout = c(2, 2))
}

\keyword{hplot}
\keyword{aplot}
