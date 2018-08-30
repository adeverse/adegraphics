\name{adegraphics-package}
\alias{adegraphics-package}
\alias{adegraphics}
\docType{package}

\title{Graphical objects for ade4 functions (and more)}

\description{
  This package was created to replace graphics functionalities of the
  \code{ade4} package and to offer customizable representations of data
  and result analysis.
  
  Graphics are objects of S4 class, which can be displayed but also stored for
  latter modifications.
  Those modifications can be graphical changes, but also superposition
  or juxtaposition of various graphical objects (creating an other type of object).
  Each object will contain graphical parameters and instructions for the
  display (calls, positions, etc.) and the data set used.
  Sometimes data is heavy, due to its size for example. Two storing
  systems exist:
  \itemize{
    \item{full storage: data is assigned to an object's slot.}
    \item{names and position: data names (as a string, obtained using
      \code{deparse(substitute)}) and their frame position (using
      \code{sys.nframe()}) are stored. Then the full data can be
      retrieve with those two informations (and only if the data objects
      are still in the environment)}
  }

  This new system is based on the \code{lattice} package and \code{grid} graphics.
}

\details{
  \tabular{ll}{
    Package: \tab adegraphics\cr
    Type: \tab Package\cr
    Version: \tab 1.0-11\cr
    Date: \tab 2018-08-30\cr
    License: \tab GPL (>=2)\cr
    Depends: \tab ade4, graphics, grid, KernSmooth, lattice, latticeExtra, methods, RColorBrewer, sp (>= 1.1-1), stats
  }
  
  A lot of classes were implemented.
  Two superclass structures the architecture in class.
  Simple and complex graphics are distinguished in the former version:
  \itemize{
    \item{\code{ADEg} class provides simple graphics using one kind of data
      (most of a time, only a data frame) and one representation method
      (points, labels, arrows...)}
    \item{\code{ADEgS} class provides complex graphics making juxtaposition, 
      superposition and/or insertion of several simple graphics.}
  }
  5 subclasses inherits from the superclass abstract \code{ADEg}:
  \itemize{
    \item{\code{ADEg.S1}: one-dimensional plot}
    \item{\code{ADEg.S2}: bi-dimensional plot}
    \item{\code{ADEg.C1}: one-dimensional data plotted in 2-D}
    \item{\code{ADEg.T}: table plot}
    \item{\code{ADEg.Tr}: triangle plot}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\references{
  Aurélie Siberchicot, Alice Julien-Laferrière, Anne-Béatrice Dufour, Jean Thioulouse and Stéphane Dray (2017). adegraphics: An S4
  Lattice-Based Package for the Representation of Multivariate Data. The R Journal. 9:2. 198--212.
  https://journal.r-project.org/archive/2017/RJ-2017-042/index.html
}

\keyword{package}

\seealso{
  \code{\link[lattice:lattice-package]{lattice}}
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEgS}}
}

\examples{
showClass("ADEg")
showClass("ADEgS")
}
