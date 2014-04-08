\name{Tr.class-class}
\docType{class}

\alias{Tr.class}
\alias{Tr.class-class}
\alias{prepare,Tr.class-method}
\alias{panel,Tr.class-method}

\title{Class \code{Tr.class}}

\description{
  A class for group representation in triangular plot.
}

\section{Objects from the Class}{
  \code{Tr.class} objects can be created by calls of the form \code{new("Tr.class", ...)}.
  
  The regular usage in this package is to use the \code{triangle.class} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{dfxyz}: the displayed values in the form of a data frame with three columns, a name or a matching call.}
      \item{\code{fac}: a factor partitionning the rows of \code{dfxyz}.}
      \item{\code{wt}: a vector of weights for \code{fac}.}
      \item{\code{labels}: a vector containing the class' labels.}
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

    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.Tr} class.
      The specific slots for \code{Tr.class} objects are: \itemize{
        \item{\code{ellipseSize}: a positive number for ellipse size.}
        \item{\code{starSize}: a number between 0 and 1 for star size.}
        \item{\code{chullSize}: \code{NULL} or a vector of numbers between 0 and 1 for the convex hulls.}
        \item{\code{col}: a \code{NULL} value, a color or a colors vector to color points, ellipses, labels, lines and polygons.}
        \item{\code{max3d} and \code{min3d}: vectors of three values for triangular maximal and minimal limits.}
        \item{\code{adjust}: a logical to adjust the device with the limits of the smaller equilateral triangle containing the values.}
    }}
    
    \item{\code{stats}}{a list of internal preliminary calculations. 
      The specific slots for \code{S2.class} objects are: \itemize{
        \item{\code{means}: a matrix containing the weighted mean calculated for each \code{fac} value.}
        \item{\code{mean2d}: a matrix containing the weighted mean calculated for each \code{fac} value on two-dimension.}
        \item{\code{covvar}: a list containing the weighted variance-covariance matrices calculated for each \code{fac} value.}
        \item{\code{covvar2d}: a list containing the weighted variance-covariance matrices calculated for each \code{fac} value on two-dimension.}
    }}
    
    \item{\code{s.misc}}{a list of some others internal parameters:
      \itemize{
        \item{\code{ellipses}: ellipses' coordinates.}
        \item{\code{chullcoord}: convex hulls' coordinates.}
    }}

    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{ADEg.Tr}}, directly.\cr
  Class \code{\linkS4class{ADEg}}, by class \code{ADEg.Tr}, distance 2.\cr
  Class \code{\linkS4class{ADEgORtrellis}}, by class \code{ADEg.Tr}, distance 3.\cr
  Class \code{\linkS4class{ADEgORADEgSORtrellis}}, by class \code{ADEg.Tr}, distance 3.
}

\section{Methods}{
  The methods of the father classes \code{"ADEg.Tr"} and \code{"ADEg"} can be used by inheritance.
  The specific methods for \code{Tr.class} are: 
  \describe{
    \item{prepare}{\code{signature(object = "Tr.class")}: 
      calls the parent method (\code{prepare} for \code{ADEg.Tr}), modifies some graphical parameters used by 
      default and calculates ellipses, convex hulls and centroids.}
      
    \item{panel}{\code{signature(object = "Tr.class")}: 
      draws arrows, labels and points.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEg.Tr}}
  \code{\link{triangle.class}}
}

\examples{
showClass("Tr.class")
}

\keyword{classes}
