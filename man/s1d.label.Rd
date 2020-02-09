\name{s1d.label}
\alias{s1d.label}

\title{1-D plot of a numeric score with labels}

\description{
  This function represents a numeric labeled score
}

\usage{
s1d.label(score, labels = 1:NROW(score), at = 0.5, poslabel = c("regular",
  "value"), facets = NULL, plot = TRUE, storeData =
  TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{score}{a numeric vector (or a data frame) used to produce the plot}
  
  \item{labels}{the labels' names drawn for each \code{score} value}
  
  \item{at}{a numeric vector used as an index}
  
  \item{poslabel}{the label position, it can be \code{regular} or \code{value}. 
    If \code{regular}, labels are evenly spaced. 
    If \code{value}, labels are placed on the weighted mean of their class.}
  
  \item{facets}{a factor splitting \code{score} so that subsets
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
  Graphical parameters for rugs are available in \code{plines} of \code{adegpar}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}

\value{
  An object of class \code{ADEg} (subclass \code{S1.label}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or data frame for \code{score} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S1.label}}
  \code{\linkS4class{ADEg.S1}}
}

\examples{
data(meau, package = "ade4")
envpca <- ade4::dudi.pca(meau$env, scannf = FALSE)
g1 <- s1d.label(envpca$l1[, 1], row.names(envpca$l1), plot = FALSE)
g2 <- s1d.label(envpca$co[, 1], row.names(envpca$co), p1d.reverse = TRUE, plot = FALSE)
G <- ADEgS(c(g1, g2), layout = c(2, 1))      
}

\keyword{aplot}
\keyword{hplot}
