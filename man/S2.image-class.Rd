\name{S2.image-class}
\docType{class}

\alias{S2.image}
\alias{S2.image-class}
\alias{prepare,S2.image-method}
\alias{panel,S2.image-method}

\title{Class \code{S2.image}}

\description{
  A class for the creation of a bi-dimensional plot with a third value represented as a continuous colored surface.
}

\section{Objects from the Class}{
  \code{S2.image} objects can be created by calls of the form \code{new("S2.image", ...)}.
  
  The regular usage in this package is to use the \code{s.image} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{dfxy}: the displayed values in the form of a data frame, a name or a matching call.}
      \item{\code{z}: a vector (or a matrix) of values on the \code{dfxy} rows.}
      \item{\code{xax}: an integer or a vector indicating the columns of \code{dfxy} kept for the x-axes.}
      \item{\code{yax}: an integer or a vector indicating the columns of \code{dfxy} kept for the y-axes.}
      \item{\code{frame}: a positive or null integer. It is the number of the frame containing the data (used 
        with \code{sys.frame(..., env = data$frame)}). Only if the data are not stored (i.e. \code{data$storeData = FALSE}).}
      \item{\code{storeData}: a logical indicating if the data should be stored in the returned object. 
        If \code{FALSE}, only the names of the data arguments are stored.}
    }}
    
    \item{\code{trellis.par}}{a list of parameters for \code{lattice} call. 
      It will be passed directly to \code{par.settings} arguments of the \code{lattice} function.}
  
    \item{\code{adeg.par}}{a list of graphical parameters, corresponding to the ones 
      given by \code{adegpar()} function.}
      
    \item{\code{lattice.call}}{a list to create the \code{trellis} object.}

    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.S2} class.
      The specific slots for \code{S2.image} objects are: \itemize{
        \item{\code{gridsize}: a 1 or 2-length vector indicating the cell numbers (horizontally and vertically) of the grid for the colored surface.}
        \item{\code{outsideLimits}: specific limits for the surface as a set of polygons. It must be an \code{SpatialPolygons} object. Hole are authorized.}
        \item{\code{span}: a value to control the degree of smoothing.}
        \item{\code{contour}: a logical to draw contour lines.}
        \item{\code{region}: a logical to fill inter-contour regions.}
        \item{\code{col}: a \code{NULL} value, a color or a colors vector used for the colored cells.}
    }}

    \item{\code{stats}}{a list of internal preliminary calculations. 
      The specific slot for \code{S2.image} objects is: \itemize{
        \item{\code{value}: a prediction value yielded by a local polynomial regression fitting.}
    }}

    \item{\code{s.misc}}{a list of some others internal parameters:
      \itemize{
        \item{\code{newgrid}: the grid expansion calculated within the prepare method.}
    }}

    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{ADEg.S2}}, directly.\cr
  Class \code{\linkS4class{ADEg}}, by class \code{ADEg.S2}, distance 2.\cr
  Class \code{\linkS4class{ADEgORtrellis}}, by class \code{ADEg.S2}, distance 3.\cr
  Class \code{\linkS4class{ADEgORADEgSORtrellis}}, by class \code{ADEg.S2}, distance 3.
}

\section{Methods}{
  The methods of the father classes \code{"ADEg.S2"} and \code{"ADEg"} can be used by inheritance.
  The specific methods for \code{S2.image} are: 
  \describe{
    \item{prepare}{\code{signature(object = "S2.image")}: 
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and calculates grid expansion and limits.}
      
    \item{panel}{\code{signature(object = "S2.image")}: 
      draws raster image.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.S2}}
  \code{\link{s.image}}
}

\examples{
showClass("S2.image")
}

\keyword{classes}
