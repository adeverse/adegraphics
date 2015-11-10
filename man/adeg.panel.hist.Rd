\name{adeg.panel.hist}
\alias{adeg.panel.hist}

\title{Panel function for adding histograms.}

\description{
  Panel function for displaying histograms into a \code{trellis} graphic (\code{lattice}
  package) and level lines.
}

\usage{
adeg.panel.hist(histValues, horizontal = TRUE, densi, drawLines, params = list(), 
  identifier = "histogramADEg")
}

\arguments{
  \item{histValues}{an object of class histogram. See \code{\link[graphics]{hist}}.}
  \item{horizontal}{a logical indicating if the plot is horizontal}
  \item{densi}{a list returns by the \code{\link[KernSmooth]{bkde}} containing the coordinates of the binned 
    kernel density estimate of the probability density of the data}
  \item{drawLines}{a vector containing the level values}
  \item{params}{graphical parameters : \code{plot.polygon}, \code{add.line} and \code{plot.line} (\code{lattice}) }
  \item{identifier}{A character string that is prepended to the name of the grob that is created.}
}

\value{
  Displays the histogram and level lines.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link[KernSmooth]{bkde}} and \code{\link[graphics]{hist}}
}

\note{
  For more information about the use of panel functions, please see the \code{lattice} package developed by Deepayan Sarkar.
}

\examples{
if(require(KernSmooth, quiet = TRUE) & require(lattice, quiet = TRUE)) {

  z <- round(rnorm(100, 30, 5))
  h <- hist(z, plot = FALSE)
  d <- bkde(z, kernel = "normal", gridsize = 60)
  l <- c(10, 20, 30, 40)
  xyplot(1:50 ~ 1:50, histValues = h, densi = d, drawLines = l, 
    panel = function(drawLines, histValues, densi){
      adeg.panel.hist(histValues = histValues, drawLines = drawLines, densi = densi)})
}
}
\keyword{aplot}