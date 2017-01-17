\name{adeg.panel.values}
\alias{adeg.panel.values}

\title{Panel function drawing a third variable into a two-dimensional scatterplot}

\description{
  Panel function for drawing coordinates with variable representation.
  The values can be represented through symbols with proportional size or various colors.
}

\usage{
adeg.panel.values(x, y, z, method, symbol, ppoints, breaks, centerpar = NULL, 
  center = 0)
}

\arguments{
  \item{x}{a numeric vector, x-coordinates for the symbols}
  
  \item{y}{a numeric vector,  y-coordinates for the symbols}
  
  \item{z}{a numeric vector, the third variable with one value per coordinates (x, y)}
  
  \item{method}{a character string equal to \code{color} or \code{size}.\cr
    If \code{color}, a palette of color is used for the symbols (one color per interval defined by \code{breaks}).\cr
    If \code{size}, symbols' area is proportional to the value. Area is 0 for values equals to \code{center}. 
    Two colors are used, one for values smaller than center and the other for values larger than center.}
    
  \item{symbol}{a character string equal to \code{square} or \code{circle}.}
  
  \item{ppoints}{a list of parameters as an extract of \code{adegpar("ppoints")}, used for points' drawing.
    \itemize{
      \item{\code{alpha}: transparency of points}
      \item{\code{cex}: size of points}
      \item{\code{col}: border color of points}
      \item{\code{pch}: symbol to use}
      \item{\code{fill}: filling color}
  }}
  
  \item{breaks}{a vector, the breaks used for splitting \code{z} if \code{method} is \code{color}}

  \item{centerpar}{a list to represent center value using elements in the \code{adegpar("ppoints")} list or \code{NULL} value.
    If the method is \code{size}, z-values equals to \code{center} have a size of zero. 
    If \code{centerpar} is not \code{NULL}, those z-values are shown as points with the \code{centerpar} drawing parameters.}
    
  \item{center}{a center value for method \code{size}}
}

\value{
  Draws the points.
}

\references{
  Tanimura, S. and Kuroiwa, C. and Mizota, T. 2006 Proportional symbol
  mapping in R \emph{Journal of Statistical Software} \bold{15}, 1--7
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\note{
  For more information about the use of panel functions, please see the \code{lattice} package developed by Deepayan Sarkar.
  
  For the symbols size, the method is \code{size} uses perceptual scaling (Tanimura et al. 2006). 
}

\examples{
if(require(lattice, quietly = TRUE)) {
param <- adegpar("ppoints")[[1]]
param$col <- adegpar("ppalette")[[1L]]$quanti(2)
z <- rnorm(10)
xyplot(1:10 ~ 1:10, panel = function(x, y, ...){
  adeg.panel.values(x, y, z, method = "size", symbol = "square", ppoints =
  param, breaks = pretty(z, 4))})
}

if(require(lattice, quietly = TRUE)) {
param$col <- adegpar()$ppalette$quali((length(pretty(z, 2)) - 1))
xyplot(1:10 ~ 1:10, panel = function(x, y, ...){
  adeg.panel.values(x, y, z, method = "color", symbol = "circle", 
                    ppoints = param, breaks = pretty(z, 2))}) 
}
}
\keyword{aplot}

