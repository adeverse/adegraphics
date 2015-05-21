\name{plotEig}

\alias{plotEig}

\title{
  Plot a barchart of eigen values
}

\description{
  This function represents a simplified barchart adapted to display eigen values. 
  The bar color depends on whether the axis is displayed, kept or not.
}

\usage{
plotEig(eigvalue, nf, xax = 1, yax = 2, col.plot = "black", col.kept = "grey", 
  col = "white", facets = NULL, plot = TRUE, storeData = FALSE, pos = -1, ...)
}

\arguments{
  \item{eigvalue}{a numeric vector of eigenvalues}
  
  \item{nf}{the number of retained factors, NULL if not provided}
  
  \item{xax}{an integer indicating which factor is plotted on the x-axis}
  
  \item{yax}{an integer indicating which factor is plotted on the y-axis}

  \item{col.plot}{a color value to fill the bar corresponding to the displayed factors}
  
  \item{col.kept}{a color value to fill the bar corresponding to the kept by not displayed factors}
    
  \item{col}{a color value to fill the bar corresponding to the other factors}
  
  \item{facets}{a factor splitting the rows of \code{dfxy} so that subsets
    of the data are represented on different sub-graphics}
    
  \item{plot}{a logical indicating if the graphics is displayed}
  
  \item{storeData}{a logical indicating if the data should be stored in
    the returned object. If \code{FALSE}, only the names of the data
    arguments are stored}
    
  \item{pos}{an integer indicating the position of the
    environment where the data are stored, relative to the environment
    where the function is called. Useful only if \code{storeData} is
    \code{FALSE}}
    
  \item{\dots}{additional graphical parameters (see
    \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}})}
}

\value{
  An object of class \code{ADEg} (subclass \code{C1.barchart}).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\details{
  Graphical parameters for bars are available in \code{ppolygons} of \code{adegpar}.
  Some appropriated graphical parameters in \code{p1d} are also available.
}


\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{C1.barchart}}
  \code{\linkS4class{ADEg.C1}}
}

\examples{
  data(microsatt, package = "ade4")
  w <- ade4::dudi.coa(data.frame(t(microsatt$tab)), scann = FALSE, nf = 3)
  g1 <- s.label(w$co, plot = FALSE)
  g2 <- plotEig(w$eig, w$nf, psub = list(text = "Eigenvalues"), pbackground = list(box = TRUE), 
    plot = FALSE)
  G <- insert(g2, g1, posi = "bottomright", ratio = 0.25)
}

\keyword{aplot}
