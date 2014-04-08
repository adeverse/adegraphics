\name{S2.class-class}
\docType{class}

\alias{S2.class-class}
\alias{S2.class}
\alias{prepare,S2.class-method}
\alias{panel,S2.class-method}

\title{Class \code{S2.class}}

\description{
  A class for group representation in bi-dimensional plot.
}

\section{Objects from the Class}{
  \code{S2.class} objects can be created by calls of the form \code{new("S2.class", ...)}.
  
  The regular usage in this package is to use the \code{s.class} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{dfxy}: the displayed values in the form of a data frame, a name or a matching call.}
      \item{\code{fac}: a factor (or a matrix of factors) splitting the rows of \code{dfxy}.}
      \item{\code{xax}: an integer or a vector indicating the columns of \code{dfxy} kept for the x-axes.}
      \item{\code{yax}: an integer or a vector indicating the columns of \code{dfxy} kept for the y-axes.}
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

    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.S2} class.
      The specific slots for \code{S2.class} objects are: \itemize{
        \item{\code{ellipseSize}: a positive number for ellipse size.}
        \item{\code{starSize}: a number between 0 and 1 for star size.}
        \item{\code{chullSize}: \code{NULL} or a vector of numbers between 0 and 1 for the convex hulls.}
        \item{\code{col}: a \code{NULL} value, a color or a colors vector to color points, ellipses, labels, lines and polygons.}
    }}
    
    \item{\code{stats}}{a list of internal preliminary calculations. 
      The specific slots for \code{S2.class} objects are: \itemize{
        \item{\code{means}: a matrix containing the weighted mean calculated for each \code{fac} value.}
        \item{\code{covvar}: a list containing the weighted variance-covariance matrices calculated for each \code{fac} value.}
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
  Class \code{\linkS4class{ADEg.S2}}, directly.\cr
  Class \code{\linkS4class{ADEg}}, by class \code{ADEg.S2}, distance 2.\cr
  Class \code{\linkS4class{ADEgORtrellis}}, by class \code{ADEg.S2}, distance 3.\cr
  Class \code{\linkS4class{ADEgORADEgSORtrellis}}, by class \code{ADEg.S2}, distance 3.
}

\section{Methods}{
  The methods of the father classes \code{"ADEg.S2"} and \code{"ADEg"} can be used by inheritance.
  The specific methods for \code{S2.class} are: 
  \describe{
    \item{prepare}{\code{signature(object = "S2.class")}: 
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by 
      default and calculates ellipses, convex hulls and centroids.}
      
    \item{panel}{\code{signature(object = "S2.class")}: 
      draws ellipses, convex hulls, stars, labels and points.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.S2}}
  \code{\link{s.class}}
}

\examples{
showClass("S2.class")
}

\keyword{classes}
