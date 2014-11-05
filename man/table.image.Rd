\name{table.image}
\alias{table.image}

\title{Heat map-like representation with colored cells}

\description{
  This function represents a two dimensional table plot in which cells are colored according with their value.
}

\usage{
table.image(dftab, coordsx = 1:ncol(as.matrix(dftab)), coordsy =
  nrow(as.matrix(dftab)):1, labelsx, labelsy, nclass = 3,
  breaks = NULL, col = NULL, plot = TRUE, storeData =
  TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dftab}{a data frame, matrix, contingency table or distance matrix used to produce the plot}
  
  \item{coordsx}{an integer or a vector indicating the columns of \code{dftab} kept}
  
  \item{coordsy}{an integer or a vector indicating the rows of \code{dftab} kept}
  
  \item{labelsx}{columns labels}
  
  \item{labelsy}{rows labels}
  
  \item{breaks}{a vector of values to split \code{dftab}. If \code{NULL}, \code{pretty(dftab, nclass)} is used.}
  
  \item{nclass}{an integer for the number of desired intervals, ignored if \code{breaks} is not missing.}
  
  \item{col}{a color or a colors vector used for the cells}
  
  \item{plot}{a logical indicating if the graphics is displayed}
  
  \item{storeData}{a logical indicating if the data should be stored in
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

\value{
  An object of class \code{ADEg} (subclass \code{T.image}) or \code{ADEgS} (if \code{add} is \code{TRUE}).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{T.image}}
  \code{\linkS4class{ADEg.T}}
}

\examples{
tab <- as.table(matrix(rnorm(900), ncol = 30))
g1 <- table.image(tab)
g2 <- table.image(tab, n = 100, coordsx = c(30, 1:29), plegend.drawKey = FALSE)

data(rpjdl, package = "ade4")
X <- data.frame(t(rpjdl$fau))
Y <- data.frame(t(rpjdl$mil))
coa1 <- ade4::dudi.coa(X, scannf = FALSE)
g3 <- table.image(Y, coordsx = rank(coa1$co[, 1]), coordsy = 1:8, nclas = 5, 
  labelsx = "", plegend.drawKey = FALSE)
}

\keyword{aplot}
\keyword{hplot}
