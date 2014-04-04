\name{insert}

\alias{insert}
\alias{insert-methods}

\alias{insert,ADEgORtrellis,missing-method}
\alias{insert,ADEgS,missing-method}
\alias{insert,ADEgORtrellis,ADEg-method}
\alias{insert,ADEgS,ADEg-method}
\alias{insert,ADEgORtrellis,ADEgS-method}
\alias{insert,ADEgS,ADEgS-method}

\title{Insert a graphic into an existing one}

\description{
  This function inserts a first graphic into a previously created and/or a displayed one.
}

\usage{
insert(graphics, oldgraphics, posi = c("bottomleft", "bottomright", "topleft", 
  "topright"), ratio = 0.2, inset = 0.0, plot = TRUE, which, dispatch = FALSE)
}

\arguments{
  \item{graphics}{an object of class \code{ADEg}, \code{ADEgS} or \code{trellis}}
  
  \item{oldgraphics}{an object of class \code{ADEg}, \code{ADEgS} or \code{missing}. If \code{oldgraphics} is \code{missing},
    \code{graphics} is added on the current device.}
  
  \item{posi}{a character value or a two-length numeric vector (in normalized parent coordinates \code{npc} from 0 to 1) indicating 
    the position of \code{olgraphics} added into \code{graphics}}
  
  \item{ratio}{a numeric value from 0 to 1 indicating the size of \code{olgraphics} regarding the plot region}
  
  \item{inset}{the inset from which the graph is drawn regarding the plot region. 
    It can be a two-length vector giving the inset in x and y. If atomic, same inset is used in x and y.}
  
  \item{plot}{a logical indicating if the graphics is displayed}
  
  \item{which}{a numeric value or a vector of values only used if \code{oldgraphics} is an \code{ADEgS} object, 
    indicating the which-th sub-graphic of \code{oldgraphics} where \code{graphics} is added.}
  
  \item{dispatch}{a logical only used if both \code{graphics} and \code{oldgraphics} are \code{ADEgS} objects with same length, 
    indicating if \code{graphics} is added one by one int \code{oldgraphics}. It is used when both \code{graphics} and \code{oldgraphics} 
    are created with \code{facets} option.}
}

\value{
  An object of class \code{"ADEgS"}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEgS}}
}

\examples{
data(deug, package = "ade4")
dd1 <- ade4::dudi.pca(deug$tab, scannf = FALSE, nf = 4)
g1 <- s.label(dfxy = dd1$li, labels = rownames(dd1$li), plabels = list(cex = 0.75), plot = FALSE)
g2 <- s1d.barchart(score = dd1$eig, plot = FALSE, 
  ppolygons = list(col = c(rep("black", 2), rep("grey", 2), rep("white", 5))), 
  p1d = list(horizontal = FALSE), psub = list(position = "topright", text = "Eigenvalues"), 
  pgrid = list(draw = FALSE), pbackground = list(box = TRUE), xlim = c(0.5, 9.5))
g1
g3 <- insert(g2, plot = FALSE)

mat <- g3@positions
mat[2, ] <- c(0.8, 0, 1, 0.2)
update(g3, positions = mat, plot = FALSE)
print(g3) ## square == NULL
print(g3, square = TRUE)
print(g3, square = FALSE)


g4 <- insert(g2, g1, posi = "topleft")

data(jv73, package = "ade4")
pca1 <- ade4::dudi.pca(jv73$morpho, scannf = FALSE)
g5 <- s.value(jv73$xy, pca1$li[, 1:2], porigin.include = FALSE)
g6 <- s.corcircle(pca1$co, pbackground.box = FALSE)
g7 <- insert(g6, g5, posi = c(0.3, 0.4, 0.5, 0.6))
}

\keyword{hplot}
\keyword{aplot}
\keyword{methods}
