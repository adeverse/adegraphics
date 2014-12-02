\name{adeg.panel.label}
\alias{adeg.panel.label}

\title{Panel function for adding labels.}

\description{
  Panel function for drawing labels into a \code{trellis} graphic (\code{lattice}
  package) with or without boxes around labels.
}

\usage{
adeg.panel.label(x, y, labels, plabels, pos = NULL)
}

\arguments{
  \item{x}{a numeric vector, x-coordinates for the labels}
  
  \item{y}{a numeric vector,  y-coordinates for the labels}
  
  \item{labels}{a vector of character string, the labels}
  
  \item{plabels}{
    a list of parameters as an extract of \code{adegpar("plabels")}, used for labels' drawing. 
    Each value can be a vector and will be recycled if necessary:
    \itemize{
      \item{\code{alpha}, \code{cex}, \code{col}: drawing parameters for the text}
      \item{\code{srt}: orientation of the labels, \code{horizontal}, \code{vertical} or an angle indication (in degrees). 
        Boxes are not rotated. If the orientation is not near to \code{horizontal}/\code{vertical} (0/90), it is best not to draw the boxes}
      \item{\code{optim}: logical. If TRUE, uses an algorithm trying to avoid labels' overlapping and outside limits}
      \item{\code{boxes}: concerns the label's boxes. a list: \itemize{
        \item{\code{draw}: logical. If TRUE, labels are framed}
        \item{\code{alpha}, \code{border}, \code{col}, \code{lwd}, \code{lty}: rule transparency, border lines and background color}
  }}}}
  
  \item{pos}{a position specifier for the text, used in panel.text.
    Values of \code{1}, \code{2}, \code{3} and \code{4} respectively indicate positions below, to the left of, 
    above and to the right of the specified coordinates.}
}

\value{
  Draws the labels.
}

\references{
  The algorithm used for labels positions optimization is inspired by the \code{pointLabel} 
  function of the \code{maptools} package developed by Tom Short.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{\link[maptools]{pointLabel}}

\note{
  For more information about the use of panel functions, please see the \code{lattice} package developed by Deepayan Sarkar.
}

\examples{
if(require(lattice, quiet = TRUE)) {
param <- adegpar("plabels")[[1]]
xyplot(1:10 ~ 1:10, panel = function(x, y, ...){
  adeg.panel.label(x, y, LETTERS[1:10], plabels = param)})

param$boxes$draw <- FALSE
param$col <- "blue"
xyplot(1:10 ~ 1:10, panel = function(x, y, ...){
  adeg.panel.label(x, y, LETTERS[1:10], plabels = param)})
}
}

\keyword{aplot}
