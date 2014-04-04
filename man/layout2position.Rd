\name{layout2position}
\alias{layout2position}

\title{Transform a layout matrix into a position one}

\description{
  This function transforms layout's informations into a position matrix useful for
  \code{ADEgS} and for \code{lattice} graphics.
}

\usage{
layout2position(mat, widths = rep(1, NCOL(mat)), heights = rep(1, NROW(mat)), ng, 
square = FALSE)
}

\arguments{
  \item{mat}{a matrix indicating the location of figures to display
    (each value must be 0 or a positive integer) or a two-length vector indicating the number of rows 
    and columns in the corresponding layout.}
    
  \item{widths}{a vector of relative values for the columns' widths on the device. Their sum must be equal to the number of columns.}
    
  \item{heights}{a vector of relative values for the rows' heights on the device. Their sum must be equal to the number of rows.}
    
  \item{ng}{a value for the number of positions needed (i.e. the number of graphics to plot)}
  
  \item{square}{a logical indicating if the graphics is an isometric plot}
}

\value{
  A four-columns matrix indicating the coordinates (in normalized parent coordinates \code{npc}) 
  of the top-right and bottom-left hand corners of each displayed figure on the device.
}

\note{
  This function is strongly inspired by the \code{layout} function in \code{graphics} package.
}
  
\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link{layout}}
}

\examples{
layout2position(mat = rbind(c(0, 0, 1), c(2, 2, 1)))
layout2position(mat = cbind(c(0, 0, 1), c(2, 2, 1)), widths = c(0.5, 1.5))
}
