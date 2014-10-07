\name{addhist}

\alias{addhist}
\alias{addhist-methods}
\alias{addhist,ADEg.S2-method}

\title{
  Adds histograms and density lines against a bi-dimensional graphics.
}

\description{
  Adds the two marginal histograms and density lines of each axis against an \code{ADEg.S2} object.  
}

\usage{
addhist(object, bandwidth, gridsize = 60, kernel = "normal", cbreaks = 2, 
  storeData = TRUE, plot = TRUE, pos = -1, ...)
}

\arguments{
  \item{object}{an \code{ADEg.S2} object
  }
  \item{bandwidth}{used for the calculations of the density lines 
    (see the \code{bkde} function of the \code{KernSmooth} package).
  }
  \item{gridsize}{used for the calculations of the density lines 
    (see the \code{bkde} function of the \code{KernSmooth} package).
  }
  \item{kernel}{used for the calculations of the density lines 
    (see the \code{bkde} function of the \code{KernSmooth} package).
  }
  \item{cbreaks}{number of cells for the histograms per interval 
    of the grid of the bi-dimensional graphics.
  }
  \item{plot}{a logical indicating if the graphics is displayed
  }
  \item{storeData}{a logical indicating if the data should be stored in
    the returned object. If \code{FALSE}, only the names of the data
    arguments are stored
  }
  \item{pos}{an integer indicating the position of the
    environment where the data are stored, relative to the environment
    where the function is called. Useful only if \code{storeData} is
    \code{FALSE}
  }
  \item{\dots}{Additional graphical parameters (see
    \code{\link{adegpar}} and \code{\link[lattice]{trellis.par.get}})
  }
}

\details{
  Density is calculated using the function \code{bkde} of the \code{KernSmooth} package.
}

\value{
  An \code{ADEgS} object, a list of four graphical objects,
  one \code{ADEg.S2} and three \code{trellis} (from \code{lattice}).
  Their names are:
  \item{\code{object}}{the \code{ADEg.S2} object}
  \item{\code{densX}}{top histogram, a \code{trellis} object}
  \item{\code{densY}}{right histogram, a \code{trellis} object}
  \item{\code{link}}{corner graphics linking the two histograms, a \code{trellis} object}
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\note{
  Into the dots arguments, the usual parameters for the \code{s.label}
  can be given with the \code{object} key.
  
  Trellis parameters are used for the three remaining graphics.
  \code{plot.polygon} handles the histogram aspect, \code{add.line} the
  graduations lines and \code{plot.line} the density lines.
  
  Finally, for the \code{link} graphic, labels aspect can be changed
  using a \code{plabels} list, as for an \code{S2.label} object.
}

\seealso{
  \code{\linkS4class{ADEg.S2}}
  \code{\linkS4class{ADEgS}}
}

\examples{
data(rpjdl, package = "ade4")
coa1 <- ade4::dudi.coa(rpjdl$fau, scannf = FALSE, nf = 4)
labli <- s.label(coa1$li)
g1 <- addhist(labli)
g2 <- addhist(labli, plabels.cex = 0, cbreaks = 3)
labco <- s.label(coa1$co)
g3 <- addhist(labco, plabels.cex = 0, cbreaks = 3)
update(g3, pbackground.col = "grey85")
}

\keyword{aplot}
