\name{plot}

\alias{kplot}
\alias{kplot.foucart}
\alias{kplot.mcoa}
\alias{kplot.mfa}
\alias{kplot.mbpcaiv}
\alias{kplot.pta}
\alias{kplot.sepan}
\alias{kplotsepan.coa}
\alias{kplot.statis}
\alias{plot}
\alias{plot.acm}
\alias{plot.betcoi}
\alias{plot.betrlq}
\alias{plot.betdpcoa}
\alias{plot.betwitdpcoa}
\alias{plot.between}
\alias{plot.coinertia}
\alias{plot.discrimin}
\alias{plot.dpcoa}
\alias{plot.fca}
\alias{plot.foucart}
\alias{plot.krandboot}
\alias{plot.krandxval}
\alias{plot.mcoa}
\alias{plot.mfa}
\alias{plot.multiblock}
\alias{plot.multispati}
\alias{plot.niche}
\alias{plot.pcaiv}
\alias{plot.procuste}
\alias{plot.randboot}
\alias{plot.randxval}
\alias{plot.rlq}
\alias{plot.pta}
\alias{plot.sepan}
\alias{plot.statis}
\alias{plot.witcoi}
\alias{plot.witdpcoa}
\alias{plot.within}
\alias{plot.witrlq}
\alias{plot.bcaloocv}
\alias{plot.discloocv}
\alias{scatter}
\alias{scatter.coa}
\alias{scatter.dudi}
\alias{scatter.nipals}
\alias{scatter.pco}
\alias{score}
\alias{score.acm}
\alias{score.mix}
\alias{score.pca}
\alias{screeplot}
\alias{screeplot.dudi}
\alias{biplot}
\alias{biplot.dudi}

\title{Methods to display the outputs of an analysis performed with \code{ade4}}

\description{
  S3 methods to display the outputs of an analysis performed with \code{ade4}
}

\usage{
\method{kplot}{foucart}(object, xax = 1, yax = 2, which.tab = 1:length(object$blo), pos = -1, 
  storeData = TRUE, plot = TRUE, \dots)
\method{kplot}{mcoa}(object, xax = 1, yax = 2, which.tab = 1:nrow(object$cov2), 
  option = c("points", "axis", "columns"), pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{kplot}{mfa}(object, xax = 1, yax = 2, which.tab = 1:length(object$blo), traject = FALSE, 
  permute = FALSE, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{kplot}{mbpcaiv}(object, xax = 1, yax = 2, which.tab =
1:length(object$blo), pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{kplot}{pta}(object, xax = 1, yax = 2, which.tab = 1:nrow(object$RV), which.graph = 1:4, 
  pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{kplot}{sepan}(object, xax = 1, yax = 2, which.tab = 1:length(object$blo), permute = FALSE, 
  traject = FALSE, posieig = "bottomleft", pos = -1, storeData = TRUE, plot = TRUE, \dots)
kplotsepan.coa(object, xax = 1, yax = 2, which.tab = 1:length(object$blo), 
  permute = FALSE, posieig = "bottomleft", pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{kplot}{statis}(object, xax = 1, yax = 2, which.tab = 1:length(object$tab.names), traject = FALSE, 
  arrow = TRUE, class = NULL, pos = -1, storeData = TRUE, plot = TRUE, \dots)

\method{plot}{acm}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{betcoi}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{betdpcoa}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{betwitdpcoa}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{betrlq}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{between}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{coinertia}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{discrimin}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{dpcoa}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{fca}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{foucart}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE,
plot = TRUE, \dots)
\method{plot}{krandboot}(x, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{krandxval}(x, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{mcoa}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{mfa}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE,
plot = TRUE, \dots)
\method{plot}{multiblock}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{multispati}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{niche}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{pcaiv}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{pta}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{procuste}(x, xax = 1, yax = 2, pos = -1, storeData =
TRUE, plot = TRUE, \dots)
\method{plot}{randboot}(x, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{randxval}(x, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{rlq}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{sepan}(x, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{statis}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{witcoi}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{witdpcoa}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{within}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{witrlq}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{bcaloocv}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)
\method{plot}{discloocv}(x, xax = 1, yax = 2, pos = -1, storeData = TRUE, plot = TRUE, \dots)

\method{scatter}{dudi}(x, xax = 1, yax = 2, permute = FALSE, posieig = "topleft", prop = FALSE, 
  density.plot = ifelse(permute, ncol(x$tab) > 1000, nrow(x$tab) > 1000), plot = TRUE, 
  storeData = TRUE, pos = -1, \dots)
\method{scatter}{coa}(x, xax = 1, yax = 2, method = 1:3, posieig = "topleft", pos = -1, 
  storeData = TRUE, plot = TRUE, \dots)
\method{scatter}{pco}(x, xax = 1, yax = 2, posieig = "topleft", pos = -1, storeData = TRUE, 
  plot = TRUE, \dots)
\method{scatter}{nipals}(x, xax = 1, yax = 2, posieig = "topleft", pos = -1, storeData = TRUE, 
  plot = TRUE, \dots)

\method{score}{acm}(x, xax = 1, which.var = NULL, type = c("points", "boxplot"), pos = -1, 
  storeData = TRUE, plot = TRUE, \dots)
\method{score}{mix}(x, xax = 1, which.var = NULL, type = c("points", "boxplot"), pos = -1, 
  storeData = TRUE, plot = TRUE, \dots)

\method{score}{pca}(x, xax = 1, which.var = NULL, pos = -1, storeData = TRUE, plot = TRUE, \dots)

\method{screeplot}{dudi}(x, col.kept = "grey", col = "white", pos = -1, plot = TRUE, \dots)

\method{biplot}{dudi}(x, pos = -1, plot = TRUE, \dots)
}

\arguments{
  \item{object, x}{objects used to select a method}
  
  \item{xax}{an integer (or a vector) indicating which column(s) of {object} or \code{x} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of {object} or \code{x} is(are) plotted on the y-axis}
  
  \item{which.tab}{a numeric vector (used in \code{kplot.*}) containing the numbers of the tables used for the analysis}
  
  \item{option}{a string of characters (only used in \code{kplot.mfa}) indicating the drawing option:
    \code{points} plot of the projected scattergram onto the co-inertia axes,
    \code{axis} projections of inertia axes onto the co-inertia axes,
    \code{columns} projections of variables onto the synthetic variables planes.}
  
  \item{which.graph}{an integer between 1 and 4 (only used in \code{kplot.pta}) indicating the drawing option.
    For each table of \code{which.tab}, are drawn:
    \code{1} the projections of the principal axes,
    \code{2} the projections of the rows,
    \code{3} the projections of the columns,
    \code{4} the projections of the principal components onto the planes of the compromise.}
  
  \item{permute}{a logical value (used in \code{kplot.sepan}, \code{kplotsepan.coa} and \code{scatter.dudi}).
    If \code{FALSE}, the rows are plotted by points or density surface and the columns by arrows. If \code{TRUE}, it is the opposite.}
    
  \item{traject}{a logical value (used in \code{kplot.sepan} and \code{kplot.statis}) 
    indicating whether the trajectories between rows should be drawn in a natural order}
  
  \item{posieig}{a character value or a two-length numeric vector (in normalized parent coordinates \code{npc} from 0 to 1) 
    or \code{none} value indicating the position of the eigenvalues bar plot (used in \code{kplot.sepan}, \code{kplotsepan.coa} and \code{scatter.*}).}

  \item{arrow}{a logical value (only used in \code{kplot.statis}) indicating whether the column factorial diagrams should be plotted}
  
  \item{class}{if not NULL, a factor of length equal to the number of the total columns of the K-tables (only used in \code{kplot.statis})}
  
  \item{prop}{a logical value (only used in \code{scatter.dudi}) indicating if the size of the arrows' labels is proportional to the analysis score.}
  
  \item{density.plot}{a logical value  (only used in \code{scatter.dudi})indicating if the points are displayed as density surface (using \code{s.density}).}

  \item{method}{an integer between 1 and 3 (only used in \code{scatter.coa}) indicating the drawing option. Are drawn:
    \code{1} rows and columns with the coordinates of lambda variance,
    \code{2} rows variance 1 and columns by averaging,
    \code{3} columns variance 1 and rows by averaging.}
  
  \item{which.var}{the numbers of the kept columns for the analysis, otherwise all columns (used in \code{score.*})}
  
  \item{type}{a string of characters (only used in \code{score.acm} and \code{score.mix}) indicating if points (\code{points}) or boxplot (\code{boxplot}) are used to represent levels of factors} 
    
  \item{col.kept}{one color value to color the kept axes in the barchart (used in \code{screeplot.dudi})}

  \item{col}{one color value to color the axes in the barchart (used in \code{screeplot.dudi})}
  
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
  Returns an \code{ADEg} or an \code{ADEgS} object.
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\references{
  See \code{ade4} website: <URL: http://pbil.univ-lyon1.fr/ADE-4/>
}

\examples{
cat("To run the example on 'topic'\n")
cat("Type in your R console: example(topic, package = 'ade4') \n")
}

\keyword{hplot}
\keyword{methods}
\keyword{multivariate}
