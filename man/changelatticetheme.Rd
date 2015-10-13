\name{changelatticetheme}
\alias{changelatticetheme}

\title{
  Change the \code{lattice} theme used for \code{adegraphics}  
}

\description{
  This function allows to modify the default theme existing for \code{adegraphics} objects.
  The created theme also affects previously created objects.
}

\usage{
changelatticetheme(...)
}

\arguments{
  \item{\dots}{\code{lattice} parameters, the same used in \code{trellis.par.set} and provided by 
    \code{trellis.par.get}. If empty, reset the theme to the \code{adegraphics} one.}
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\note{
  The \code{adegraphics} theme removes all margins, sets a transparent background and grey regions.

  A further development will be the creation of various themes for \code{adegraphics}.
}

\seealso{
  \code{\link[lattice]{trellis.par.get}}
  \code{\link[lattice]{trellis.par.set}}
  \code{\link[lattice]{show.settings}}
}

\examples{
if(require(lattice, quiet = TRUE)) {
show.settings()
changelatticetheme(list(superpose.symbol = list(pch = c(21, 22, 35), cex = 1)))
show.settings()
show.settings()[1]
}
}
\keyword{iplot}
