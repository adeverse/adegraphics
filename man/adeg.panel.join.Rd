\name{adeg.panel.join}
\alias{adeg.panel.join}

\title{Panel function for joining lines.}

\description{
  Panel function for drawing lines as part of a circle centred in (0, 0) into a \code{trellis} graphic (\code{lattice}
  package).
}

\usage{
adeg.panel.join(drawLines, params = list())
}

\arguments{
  \item{drawLines}{a vector containing the level values used as radius of the circle}
  \item{params}{graphical parameters : \code{plabels} and \code{add.line} (\code{lattice})}
}

\value{
  Displays level lines and their values.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\note{
  For more information about the use of panel functions, please see the \code{lattice} package developed by Deepayan Sarkar.
}

\examples{
if(require(lattice, quiet = TRUE)) {
  xyplot(0:20 ~ 0:20, drawLines = c(5, 10, 15), params = list(plabels.cex = 2), 
    panel = function(drawLines, params){
      adeg.panel.join(drawLines = drawLines, params = params)})
}}

\keyword{aplot}