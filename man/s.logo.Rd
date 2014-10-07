\name{s.logo}
\alias{s.logo}

\title{2-D scatter plot with logos (bitmap objects)}

\description{
  This function represents a two dimensional scatter plot associating logos with points.
}

\usage{
s.logo(dfxy, logos, xax = 1, yax = 2, facets = NULL, 
  plot = TRUE, storeData = TRUE, add = FALSE, pos = -1, ...)
}

\arguments{
  \item{dfxy}{a data frame used to produce the plot}

  \item{logos}{a list containing the picture to use for each point}
  
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
  An object of class \code{ADEg} (subclass \code{S2.logo}) or \code{ADEgS} (if \code{add} is \code{TRUE} and/or 
  if facets or vectors for \code{xax}/\code{yax} are used).\cr
  The result is displayed if \code{plot} is \code{TRUE}.
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{S2.logo}}
  \code{\linkS4class{ADEg.S2}}
}

\examples{
data(ggtortoises, package = "ade4")
g1 <- s.logo(ggtortoises$pop, 
  ggtortoises$ico[as.character(ggtortoises$pop$carap)], 
  pori.incl = FALSE, ppoints.cex = 0.5)
g1 <- s.label(ggtortoises$pop, add = TRUE, plabels.boxes.alpha = 0)

g2 <- s.logo(ggtortoises$pop, 
  ggtortoises$ico[as.character(ggtortoises$pop$carap)], 
  Sp = ggtortoises$Spatial, pback.col = "lightblue", pSp.col = "white", 
  pgrid.draw = FALSE, ppoints.cex = 0.5)
g2 <- s.label(ggtortoises$misc, pgrid.draw = FALSE, 
  porigin.include = FALSE, paxes.draw = FALSE, add = TRUE)

data(capitales, package = "ade4")
g3 <- s.logo(capitales$xy[sort(rownames(capitales$xy)), ], capitales$logo, 
  Sp = capitales$Spatial, pback.col = "lightblue", pSp.col = "white", 
  pgrid.draw = FALSE)
}
\keyword{aplot}
\keyword{hplot}
