\name{C1.gauss-class}
\docType{class}

\alias{C1.gauss}
\alias{C1.gauss-class}
\alias{prepare,C1.gauss-method}
\alias{panel,C1.gauss-method}

\title{Class \code{C1.gauss}}

\description{
  A class for the creation and display of a numeric score using gauss' curves.
}

\section{Objects from the Class}{
  \code{C1.gauss} objects can be created by calls of the form \code{new("C1.gauss", ...)}.
  
  The regular usage in this package is to use the \code{s1d.gauss} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{score}: the displayed values in the form of a numeric vector, a name or a matching call.}
      \item{\code{fac}: a factor for \code{score} splitting in the form of a vector, a factor, a name or a matching call.}
      \item{\code{wt}: a vector of weights for \code{score}}
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

    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.C1} class.
      The specific slots for \code{C1.gauss} objects are: \itemize{
      \item{\code{fill}: a logical to yield the gauss curves transparent.}
      \item{\code{col}: a logical, a color or a colors vector to color labels, rugs, lines and polygons.}
      \item{\code{steps}: a value for the number of segments used to draw Gauss curves.}
    }}
    
    \item{\code{stats}}{a list of internal preliminary calculations. 
      The specific slots for \code{C1.gauss} objects are: \itemize{
      \item{\code{means}: the weighted mean calculated for each \code{fac} value.}
      \item{\code{var}: the weighted variance calculated for each \code{fac} value.}
      \item{\code{gausscurves}: the density gauss curve calculated for each \code{fac} value.}
    }}
    
    \item{\code{s.misc}}{a list of some others internal parameters.
      The specific slot for \code{C1.gauss} objects is: \itemize{
      \item{\code{rug}: an index value indicating where the rugs are drawn}
    }}
    
    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{ADEg.C1}}, directly.\cr
  Class \code{\linkS4class{ADEg}}, by class \code{ADEg.C1}, distance 2.\cr
  Class \code{\linkS4class{ADEgORtrellis}}, by class \code{ADEg.C1}, distance 3.\cr
  Class \code{\linkS4class{ADEgORADEgSORtrellis}}, by class \code{ADEg.C1}, distance 3.
}

\section{Methods}{
  The methods of the father classes \code{"ADEg.C1"} and \code{"ADEg"} can be used by inheritance.
  The specific methods for \code{C1.gauss} are: 
  \describe{
    \item{prepare}{\code{signature(object = "C1.gauss")}: 
      calls the parent method (\code{prepare} for \code{ADEg.C1}), modifies some graphical parameters used by default and 
      calculates the Gauss curves according to the numeric score and the values' categories (using weighted mean and standard deviation).}
      
    \item{panel}{\code{signature(object = "C1.gauss")}: 
      draws Gauss curves and level names of each curve.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.C1}}
  \code{\link{s1d.gauss}}
}

\examples{
showClass("C1.gauss")
}

\keyword{classes}
