\name{plot.inertia}

\alias{plot.inertia}
\alias{score.inertia}

\title{Display the decomposition of inertia which measure the contributions of 
  rows/columns in mutivariate methods}

\description{
  S3 method to display the decomposition of inertia (\code{inertia} object) 
  which measure the contributions of rows/columns in mutivariate methods 
  (\code{dudi} objects from \code{ade4})
}

\usage{
\method{plot}{inertia}(x, xax = 1, yax = 2, threshold = 0.1, 
  contrib = c("abs", "rel"), type = c("label", "cross", "ellipse", "both"), 
  ellipseSize = 1.5, posieig = "none", plot = TRUE, 
  storeData = TRUE, pos = -1, \dots)
\method{score}{inertia}(x, xax = 1, threshold = 0.1, contrib = c("abs", "rel"), 
  posieig = "none", pos = -1, storeData = TRUE, plot = TRUE, \dots)
}

\arguments{
  \item{x}{an object of the \code{dudi} class; it must be the output of a 
    correspondance analysis (\code{coa} object).}
  
  \item{xax}{an integer indicating which column of {x} is plotted on the x-axis}
  
  \item{yax}{an integer indicating which column of {x} is plotted on the y-axis.
    If \code{yax} is equal to \code{xax}, a one-dimensional graph is display.}
  
  \item{threshold}{a numeric value containing the contribution threshold 
    (between 0 and 1) at which points should be drawn on the graphic. Low 
    contribution points will be represented by a grey point and without label. 
    When the contributions are displayed on a single axis, a dotted line 
    describes the contribution threshold.}
    
  \item{contrib}{a character value indicating which contributions are plotted: 
    \code{abs} for absolute contributions (rows/columns involved in the factor
    axis/map construction) and \code{rel} for relative contribution (quality of
    rows/columns representation on the factor axis/map).}
  
  \item{type}{a character value indicating which type represents contribution. 
  Labels size (\code{label}), crosses size(\code{cross}) or ellipses size 
  (\code{ellipse}) can be proportional to the contributions. If \code{type} is 
  \code{both}, crosses and ellipses both have sizes proportional to the 
  contributions.}
  
  \item{ellipseSize}{a positive number for ellipse size when \code{type} is 
    \code{ellipse}}
  
  \item{posieig}{a character value or a two-length numeric vector (in normalized 
    parent coordinates \code{npc} from 0 to 1) or \code{none} value indicating 
    the position of the eigenvalues bar plot.}

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
  Returns an \code{ADEgS} object.
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Clément Claustre, Anne-Béatrice Dufour, Aurélie Siberchicot 
  \email{aurelie.siberchicot@univ-lyon1.fr} and Stéphane Dray
}

\examples{
# First example
data(bf88, package = "ade4")
coa1 <- ade4::dudi.coa(bf88$S1, scannf = FALSE, nf = 2)

###### row=T / col=F
res11 <- ade4::inertia(coa1, row = TRUE, col = FALSE, nf = 2)
g11 <- plot(res11, threshold = 0.06)
g12 <- plot(res11, threshold = 0.06, plabels.boxes.draw = TRUE, plines.lwd = 0,
  light_row.ppoints.cex = 0, posieig = "bottomleft")
g13 <- score(res11, threshold = 0.06)
names(g13)
g14 <- score(res11, xax = 2, threshold = 0.06)

###### row=F / col=T
res12 <- ade4::inertia(coa1, row = FALSE, col = TRUE, nf = 2)
res12$col.abs
idx <- which(res12$col.abs[, 1]/100 >= 0.1 | res12$col.abs[, 2]/100 >= 0.1)
rownames(res12$col.abs[idx, ])
coa1$co[idx, ]
g15 <- plot(res12)
g16 <- score(res12, threshold = 0.08)
g17 <- score(res12, threshold = 0.07)


########################################
########################################
# Second example
data(housetasks, package = "ade4")
coa2 <- ade4::dudi.coa(housetasks, scann = FALSE)

###### row=T / col=F
res21 <- ade4::inertia(coa2, row = TRUE, col = FALSE)
g21 <- plot(res21)
g22 <- score(res21)
g23 <- score(res21, xax = 2)

###### row=F / col=T
res22 <- ade4::inertia(coa2, row = FALSE, col = TRUE)
g24 <- plot(res22, plabels.cex = 2)
names(g24)
g25 <- plot(res22, posieig = "topleft")
names(g25)
g26 <- plot(res22, heavy_col.plabels.box.draw = TRUE, 
  light_col.ppoints.col = "purple")
g27 <- plot(res22, type = "both")
g28 <- plot(res22, type = "ellipse", ellipseSize = 3, plabels.col = "black", 
  pellipse.col = "purple", pellipses.border = "black")
}

\keyword{hplot}
\keyword{methods}
