\name{scatter.dudi}
\alias{scatter.dudi}

\title{Factorial map plot}

\description{
  This function performs the scatter diagram of \code{dudi} object to show analysis results.
  It represents a factorial map of individuals and the projection of the vectors of the canonical basis (re-scaled).
}

\usage{
scatter.dudi(x, xax = 1, yax = 2, permute = FALSE, posieig = "topleft",
  prop = FALSE, density.plot = ifelse(permute, ncol(x$tab) > 1000, 
  nrow(x$tab) > 1000), plot = TRUE, storeData = FALSE, pos = -1, ...)
}

\arguments{
  \item{x}{an object of class \code{dudi}}
  
  \item{xax}{an integer indicating which column of \code{x} is plotted on the x-axis}
  
  \item{yax}{an integer indicating which column of \code{x} is plotted on the y-axis}
  
  \item{permute}{a logical. 
    If \code{FALSE}, the rows are plotted by points or density surface and the columns by arrows. 
    If \code{TRUE}, it is the opposite.}
    
  \item{posieig}{a character value or a two-length numeric vector (in normalized parent coordinates \code{npc} from 0 to 1) 
    or \code{none} value indicating the position of the eigenvalues bar plot.}
  
  \item{prop}{a logical indicating if the size of the arrows' labels is proportional to the
    analysis score.}
  
  \item{density.plot}{a logical indicating if the points are displayed as density surface (using \code{s.density}).}

  \item{plot}{a logical indicating if the graphics is displayed}
  
  \item{storeData}{a logical indicating if the data should be stored in
    the returned object. If \code{FALSE}, only the names of the data
    arguments are stored}
    
  \item{pos}{an integer indicating the position of the
    environment where the data are stored, relative to the environment
    where the function is called. Useful only if \code{storeData} is
    \code{FALSE}}
  
  \item{\dots}{additional graphical parameters (see \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}}) applied on one 
    among three graphical objects that compose the returned \code{ADEgS}.
    Parameters for the \code{s.arrow} plot are available with the key \code{col}, ones for the \code{s.density} or
    \code{s.label} plot with the key \code{row} and ones for the eigenvalues bar plot with the key \code{eigen}.}
}

\value{
  Returns an \code{ADEgS} object.
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link{s.label}}
  \code{\link{s.density}}
  \code{\link{s.arrow}}
  \code{\link{screeplot}}
}

\examples{
data(deug, package = "ade4")
dd1 <- ade4::dudi.pca(deug$tab, scannf = FALSE, nf = 4)
g1 <- scatter.dudi(dd1, posieig = "bottomright")

data(rhone, package = "ade4")
dd2 <- ade4::dudi.pca(rhone$tab, scannf = FALSE, nf = 4)
g2 <- scatter.dudi(dd2, row.sub = "Principal component analysis")
}

\keyword{hplot}
