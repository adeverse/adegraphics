\name{s.label}
\alias{s.label}

\title{2-D scatter plot with labels}

\description{
  This function represents a two dimensional scatter plot associating labels with points.
}

\usage{
s.label(dfxy, labels = rownames(dfxy), xax = 1, yax = 2,
  facets = NULL, plot = TRUE, storeData = TRUE, add = FALSE, 
  pos = -1, ...)
}

\arguments{
  \item{dfxy}{a data frame used to produce the plot}
  
  \item{labels}{a vector of character strings for the points' labels}
  
  \item{xax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the x-axis}
  
  \item{yax}{an integer (or a vector) indicating which column(s) of \code{dfxy} is(are) plotted on the y-axis}

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
    where the function is called. Useful only if \code{storeData} is
    \code{FALSE}}
    
  \item{\dots}{additional graphical parameters (see
    \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}})}
}

\value{
  An object of class \code{ADEg} (subclass \code{S2.label}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or vectors for \code{xax}/\code{yax} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.label}}
  \code{\linkS4class{ADEg.S2}}
}

\examples{
x0 <- runif(50, -2, 2)
y0 <- runif(50, -2, 2)
z <- x0 ^ 2 + y0 ^ 2
g1 <- s.label(data.frame(x0, y0), label = as.character(z < 1), paxes.draw = TRUE, 
  axis.text = list(col = "grey"))
  
data(mafragh, package = "ade4")
g2 <- s.label(mafragh$xy, nb = mafragh$nb, paxes.draw = FALSE)

data(irishdata, package = "ade4")
g3 <- s.label(irishdata$xy.utm, Sp = irishdata$Spatial.contour)   
## update irishdata$xy.utm call to irishdata$xy

\dontrun{data(atlas, package = "ade4")
  g4 <- s.label(atlas$xy, lab = atlas$names.district, Sp = atlas$Spatial.contour)
  g5 <- s.label(atlas$xy, lab = atlas$names.district, Sp = atlas$Spatial)
}
}
\keyword{aplot}
\keyword{hplot}
