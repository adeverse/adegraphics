\name{superpose}

\alias{superpose}
\alias{superpose-methods}
\alias{superpose,ADEgORtrellis,ADEgORtrellis,ANY,ANY-method}
\alias{superpose,ADEgS,ADEgORtrellis,numeric,logical-method}
\alias{superpose,ADEgS,ADEgORtrellis,numeric,ANY-method}
\alias{superpose,ADEgS,ADEgORtrellis,missing,ANY-method}
\alias{superpose,ADEgS,ADEgS,missing,ANY-method}

\alias{+-methods}
\alias{\S4method{+}{ADEg}}
\alias{+,ADEg,ADEg-method}
\alias{+,ADEg,ADEgS-method}
\alias{+,ADEgS,ADEg-method}

\title{Superpose two graphics}

\description{
  This function superposes two graphics and extends the graphical constraints of a first graphic to a second one.
}

\usage{
superpose(g1, g2, which, plot = FALSE)
\S4method{+}{ADEg}(e1, e2)
}

\arguments{
  \item{g1}{an object of class \code{ADEg}, \code{ADEgS} or \code{trellis}}
  
  \item{g2}{an object of class \code{ADEg}, \code{ADEgS} or \code{trellis} superposed on \code{g1}}
  
  \item{e1}{an object of class \code{ADEg} or \code{ADEgS}}
  
  \item{e2}{an object of class \code{ADEg} or \code{ADEgS} superposed on \code{e1}}
  
  \item{which}{if \code{g1} is an \code{ADEgS}, which \code{ADEg} is used as the base of superposition 
  (\code{g2} is superposed on \code{g1[[which]]})}
  
  \item{plot}{a logical indicating if the graphics is displayed}
}


\details{
  The created \code{ADEgS} object is a layout of two graphical objects. 
  Each of the two objects superposed still have its graphical parameters in the created layout.
  However, the \code{ADEgS} displayed favour the graphical parameters of the object below : 
  displayed limits, grid, legend and axes are those of \code{g1} (respectively \code{e1}) and \code{g2} 
  (respectively \code{e2}) has transparent background and labels' boxes.
  
  The \code{superpose} method is defined for: \itemize{
    \item{\code{signature(g1 = "ADEgS", g2 = "ADEg", which = "numeric",	plot = "logical")}}
    \item{\code{signature(g1 = "ADEgS", g2 = "ADEg", which = "numeric", plot = "ANY")}}
    \item{\code{signature(g1 = "ADEgS", g2 = "ADEg", which = "missing",	plot = "ANY")}: 
      If \code{which} is \code{missing}, the last \code{ADEg} of \code{g1@ADEglist} is used as the base of superposition. In that case, \code{which = length(g1)}}
    \item{\code{signature(g1 = "ADEgORtrellis", g2 = "ADEgORtrellis", which = "ANY", plot = "ANY")}: 
      If \code{g1} is an \code{ADEg} object, no \code{which} is needed.}
    \item{\code{signature(g1 = "ADEgS", g2 = "ADEgS", which = "missing", plot = "ANY")}}
  }
  
  The \code{+} method is defined for: \itemize{
    \item{\code{signature(e1 = "ADEg", e2 = "ADEg")}: superpose e2 on e1}
    \item{\code{signature(e1 = "ADEg", e2 = "ADEgS")}: superpose e2 to e1}
    \item{\code{signature(e1 = "ADEgS", e2 = "ADEg")}: calls the \code{+} method with signature 
      \code{(e1 = "ADEg", e2 = "ADEgS")}.}
  }
}

\value{
  An object of class \code{"ADEgS"}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link{add.ADEg}}
  \code{\linkS4class{ADEgS}}
  \code{\linkS4class{ADEg}}
}

\examples{
cha <- LETTERS[1:20]
xy <- cbind.data.frame(runif(length(cha)), runif(length(cha)))
g1 <- s.label(xy, labels = cha, ppoints.alpha = 0, pbackground.col = "grey85")
g2 <- s.label(xy, labels = cha, plabels.cex = 0, paxes.draw = TRUE, ppoints.pch = 4, 
  ppoints.col = "red")
g3 <- superpose(g1, g2, plot = TRUE)
g4 <- superpose(g2, g1, plot = TRUE)

data(jv73, package = "ade4")
pca1 <- ade4::dudi.pca(jv73$morpho, scannf = FALSE)
g5 <- s.label(pca1$li, plabels.optim = TRUE)
g6 <- s.class(pca1$li, jv73$fac.riv, starSize = 0, ellipseSize = 0, chullSize = 1, 
  ppolygons.alpha = 0.4, col = rainbow(12), ppoints.cex = 0)
g5 + g6

\dontrun{g7 <- s.label(pca1$li, plabels.optim = TRUE, facets = jv73$fac.riv, plot = FALSE)
g8 <- s.class(pca1$li, jv73$fac.riv, facets = jv73$fac.riv, starSize = 0, chullSize = 1, 
  ellipseSize = 0, ppolygons.alpha = 0.4, col = rainbow(12), ppoints.cex = 0, plot = FALSE)
g9 <- superpose(g7, g8, plot = TRUE)
}
}

\keyword{hplot}
\keyword{methods}
