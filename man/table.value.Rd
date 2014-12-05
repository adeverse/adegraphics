\name{table.value}
\alias{table.value}

\title{Heat map-like representation with proportional symbols}

\description{
  This function represents a two dimensional table plot with proportional or colored squares or circles for each value.
}

\usage{
table.value(dftab, coordsx = 1:ncol(as.matrix(dftab)), coordsy =
nrow(as.matrix(dftab)):1, labelsx, labelsy, breaks = NULL, method =
c("size", "color"), symbol = c("square", "circle", "diamond",
"uptriangle", "downtriangle"), col = NULL, nclass = 3, center = 0,
centerpar = NULL, plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dftab}{a data frame, matrix, contingency table or distance matrix used to produce the plot}
  
  \item{coordsx}{an integer or a vector indicating the columns of \code{dftab} kept}
  
  \item{coordsy}{an integer or a vector indicating the rows of \code{dftab} kept}

  \item{labelsx}{columns labels}
  
  \item{labelsy}{rows labels}

  \item{breaks}{a vector of values to split \code{dftab}. If \code{NULL}, \code{pretty(dftab, nclass)} is used.}
  
  \item{method}{\code{color} or \code{size} value for represent \code{z}.   
    If \code{color}, a palette of color is used for the symbols (one color per interval).
    If \code{size}, symbols of proportional area are used. Area is 0 for values
    equals to center (default 0). Two colors are used, for values less than center and larger than center.}
  
  \item{symbol}{value for symbol type}
  
  \item{col}{a color or a colors vector to color symbols.
    If \code{method} is \code{size}, a 2-length vector of color is expected. 
    If \code{method} is \code{color}, it must have as many colors as the number of class}
  
  \item{nclass}{an integer for the number of desired intervals, ignored if \code{breaks} is not missing.}
  
  \item{center}{a center value for method \code{size}}
    
  \item{centerpar}{a logical or a list to represent center value using elements in the 
    \code{adegpar("ppoints")} list}
    
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
  An object of class \code{ADEg} (subclass \code{T.cont} if \code{dftab} is an \code{table} object, 
  otherwise subclass \code{T.value}) or \code{ADEgS} (if \code{add} is \code{TRUE}).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\note{
  For the symbol size, if the method is \code{size}, we use perceptual scaling (Tanimura et al. 2006) .  
}

\references{
  Tanimura, S. and Kuroiwa, C. and Mizota, T. 2006 Proportional symbol
  mapping in R \emph{Journal of Statistical Software} \bold{15}, 1--7 
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{T.value}}
  \code{\linkS4class{T.cont}}
  \code{\linkS4class{ADEg.T}}
}

\examples{
## data.frame
data(olympic, package = "ade4")
w <- olympic$tab
w <- data.frame(scale(w))
wpca <- ade4::dudi.pca(w, scann = FALSE)
g1 <- table.value(w, ppoints.cex = 0.5, axis.line = list(col = "darkblue"), 
  axis.text = list(col = "darkgrey"))
g2 <- table.value(w, coordsy = rank(wpca$li[, 1]), ppoints.cex = 0.5, 
  axis.line = list(col = "darkblue"), axis.text = list(col = "darkgrey"))
g3 <- table.value(w, coordsy = wpca$li[, 1], coordsx = wpca$co[, 1], ppoints.cex = 0.5, 
  axis.line = list(col = "darkblue"), axis.text = list(col = "darkgrey"))

## distance
data(eurodist)
g5 <- table.value(eurodist, symbol = "circle", 
  ptable.margin = list(bottom = 5, top = 16, left = 5, right = 16))

\dontrun{
## table
data(rpjdl, package = "ade4")
w <- data.frame(t(rpjdl$fau))
wcoa <- ade4::dudi.coa(w, scann = FALSE)
g6 <- table.value(as.table(as.matrix(w)), meanY = TRUE, coordsx = wcoa$c1[,1], 
  coordsy = rank(wcoa$l1[,1]), ppoints.cex = 0.2, labelsx = "", col = "black")
}
}

\keyword{aplot}
\keyword{hplot}
