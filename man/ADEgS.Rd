\name{ADEgS}
\alias{ADEgS}

\title{Creation of \code{ADEgS} objects}

\description{
  Creates and displays an \code{"ADEgS"} object, a set of \code{ADEg}, \code{trellis} and/or \code{ADEgS} objects,
  managed by superposition, insertion and/or juxtaposition.
}

\usage{
ADEgS(adeglist, positions, layout, add = NULL, plot = TRUE)
}

\arguments{
  \item{adeglist}{a list of several \code{trellis}, \code{ADEg} and/or \code{ADEgS} objects.}
  
  \item{positions}{a matrix with four columns and as many rows as the number of graphical objects in \code{ADEglist} slot.
      For each simple graphic, i.e. in each row, the coordinates of the top-right and the bottom-left hand corners are in \code{npc} unit (normalized parent coordinates).}
      
  \item{layout}{a layout indication in two possible forms: \itemize{
    \item{a list containing arguments of the \code{layout} function}
    \item{a two-length vector containing rows' and columns' number of layout}
  }}
  
  \item{add}{a square matrix with as many rows and columns as the number of graphical objects in the \code{ADEglist} slot.
      The value at the i-th row and j-th column is equal to 1 whether the j-th graphical object in \code{ADEglist} slot is superpose to i-th graphical one.
      Otherwise, this value is equal to 0.}
      
  \item{plot}{a logical. If the graphics should be displayed}
}

\value{
  an \code{ADEgS} object. If \code{plot = TRUE}, the created object is displayed.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEgS}}
}

\examples{
xy <- matrix(rnorm(20), ncol = 2)
g1 <- s.label(xy)
g2 <- s.class(xy, fac = as.factor(rep(LETTERS[1:2], length.out = 10)), ppoints.cex = 0,
  col = c("blue", "red"))
g3 <- ADEgS(list(g1, g2), rbind(c(0, 0, 0.5, 1), c(0.5, 0, 1, 1)))
g4 <- ADEgS(list(g1, g2), layout = c(2, 1))
g5 <- ADEgS(list(g1, g2))
g6 <- ADEgS(list(g1, g2), add = matrix(c(0, 1, 0, 0), byrow = TRUE, ncol = 2))

data(olympic, package = "ade4")
dudi1 <- ade4::dudi.pca(olympic$tab, scan = FALSE)
g7 <- s.arrow(dudi1$li)
g8 <- s.corcircle(dudi1$co, lab = names(olympic$tab))
g9 <- ADEgS(list(g7, g8), rbind(c(0, 0, 0.5, 1), c(0.5, 0, 1, 1)))
g9[[1]]
g9[1, drop = FALSE]
length(g9)
}

\keyword{hplot}
