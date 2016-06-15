\name{s.value}
\alias{s.value}

\title{2-D scatter plot with proportional symbols (bubble plot)}

\description{
  This function represents a two dimensional scatter plot with a third value represented by symbols.
}

\usage{
s.value(dfxy, z, breaks = NULL, xax = 1, yax = 2, method = c("size",
"color"), symbol = c("square", "circle", "diamond", "uptriangle", "downtriangle"),
col = NULL, nclass = 4, center = 0, centerpar = NULL, facets = NULL,
plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxy}{a data frame used to produce the plot}

  \item{z}{a vector (or a matrix) with as many values as rows in \code{dfxy}}

  \item{breaks}{a vector containing the breaks used for splitting \code{z} value. If \code{NULL}, \code{pretty(z, n)} is used.}
  
  \item{xax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the y-axis}

  \item{method}{\code{color} or \code{size} value for represent \code{z}.   
    If \code{color}, a palette of color is used for the symbols (one color per interval).
    If \code{size}, symbols of proportional area are used. Area is 0 for values
    equals to center (default 0). Two colors are used, for values less than center and larger than center.}
    
  \item{symbol}{value for symbol type}
  
  \item{col}{a color or a colors vector to color symbols.
    If \code{method} is \code{size}, a 2-length vector of color is expected. 
    If \code{method} is \code{color}, it must have as many colors as the number of class.}
    
  
  \item{nclass}{an integer for the number of desired intervals, ignored if \code{breaks} is not missing.}
  
  \item{center}{a center value for method \code{size}}
  
  \item{centerpar}{a logical or a list to represent center value using elements in the 
    \code{adegpar("ppoints")} list}
  
  \item{facets}{a factor splitting the rows of \code{dfxy} so that subsets
    of the data are represented on different sub-graphics}
    
  \item{plot}{a logical indicating if the graphics is displayed}
  
  \item{storeData}{a logical indicating if the data should be stored in
    the returned object. If \code{FALSE}, only the names of the data
    arguments are stored}
    
  \item{add}{a logical. If \code{TRUE}, the graphic is superposed to the graphics
    already plotted in the current device}
    
  \item{pos}{an integer indicating the position of the
    environment where the data are stored, relative to the environment
    where the function is called. Useful only if 
    \code{storeData} is \code{FALSE}}
    
  \item{\dots}{additional graphical parameters (see
    \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}})}
}

\value{
  An object of class \code{ADEg} (subclass \code{S2.value}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or multidimensional \code{z} or vectors for \code{xax}/\code{yax} are used).\cr
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
  \code{\linkS4class{S2.value}}
  \code{\linkS4class{ADEg.S2}}
}

\examples{ 
data(rpjdl, package = "ade4")
fau.coa <- ade4::dudi.coa(rpjdl$fau, scan = FALSE, nf = 3)
g1 <- s.value(fau.coa$li, fau.coa$li[,3])
update(g1, key = list(space = "right", columns = 1))
g2 <- s.value(fau.coa$li, fau.coa$li[,3], method = "color", plegend.size = 0.8)
g3 <- s.value(fau.coa$li, fau.coa$li[,3], plegend.size = 0.8, symbol = "square",
   method = "color",  col = colorRampPalette(c("yellow", "blue"))(6))
g4 <- s.value(fau.coa$li, fau.coa$li[,3], plot = FALSE)
g5 <- s.value(fau.coa$li, fau.coa$li[, 3], center = 0, method = "size", 
  symbol = "circle", col = c("yellow", "red"), plot = FALSE)
g6 <- ADEgS(c(g4, g5), positions = layout2position(matrix(c(1, 2), 1, 2)), 
  add = matrix(0, ncol = 2, nrow = 2))  
   
data(irishdata, package = "ade4")
irq0 <- data.frame(scale(irishdata$tab, scale = TRUE))
g7 <- s.value(irishdata$xy.utm, irq0, Sp = irishdata$Spatial.contour, paxes.draw = FALSE, 
  pgrid.draw = FALSE, pSp.alpha = 0.4)
}

\keyword{aplot}
\keyword{hplot}
