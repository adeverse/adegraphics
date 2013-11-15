\name{zoom}

\alias{zoom}
\alias{zoom-methods}
\alias{zoom,ADEg.S1,numeric,missing-method}
\alias{zoom,ADEg.S1,numeric,numeric-method}
\alias{zoom,ADEg.S2,numeric,missing-method}
\alias{zoom,ADEg.S2,numeric,numeric-method}

\title{Zoom in or out}

\description{
  This function performs a zoom on a \code{ADEg.S1} or \code{ADEg.S2} displayed object.
}

\usage{
zoom(object, zoom, center)
}

\arguments{
  \item{object}{a \code{ADEg.S1} or \code{ADEg.S2} object}
  \item{zoom}{a numeric value to zoom in (if \code{zoom} > 1) or out (if \code{zoom} < 1)}
  \item{center}{a numeric value (if \code{object} is a \code{ADEg.S1} object) or a two-length vector (if \code{object} is a \code{ADEg.S2} object) 
    as a reference point to zoom (in or out). If it is \code{missing}, the displayed center point is used.}
}

\value{
  Updated display after zoom.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg.S2}}
  \code{\linkS4class{ADEg.S1}}
}

\examples{
data(olympic, package = "ade4")
dudi1 <- ade4::dudi.pca(olympic$tab, scan = FALSE)
g <- s.corcircle(dudi1$co, lab = names(olympic$tab), fullcircle = TRUE, psub.text = "data:olympic")
zoom(g, 0.5)
zoom(g, 2, center = c(-0.4, 0.8))
}

\keyword{iplot}

