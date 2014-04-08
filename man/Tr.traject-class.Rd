\name{Tr.traject-class}
\docType{class}

\alias{Tr.traject}
\alias{Tr.traject-class}
\alias{prepare,Tr.traject-method}
\alias{panel,Tr.traject-method}

\title{Class \code{Tr.traject}}

\description{
  A class for the creation and display of triangular plot with trajectories linking the points.
}

\section{Objects from the Class}{
  \code{Tr.traject} objects can be created by calls of the form \code{new("Tr.traject", ...)}.
  
  The regular usage in this package is to use the \code{triangle.traject} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{dfxyz}: the displayed values in the form of a three columns data frame, a name or a matching call.}
      \item{\code{fac}: a factor (or a matrix of factors) splitting the rows of \code{dfxyz}.}
      \item{\code{labels}: a vector of character strings containing the trajectories' labels.}
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
      The specific slots for \code{Tr.traject} objects are: \itemize{
        \item{\code{max3d} and \code{min3d}: vectors of three values for triangular maximal and minimal limits.}
        \item{\code{adjust}: a logical to adjust the device with the limits of the smaller equilateral triangle containing the values}
        \item{\code{order}: a vector containing the drawing order of the trajectories. A vector of length equal to factor.}
        \item{\code{col}: a \code{NULL} value, a color or a colors vector to color points, labels and lines.}
        
    }}

    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters}
    
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
  The specific methods for \code{Tr.traject} are: 
  \describe{
    \item{prepare}{\code{signature(object = "Tr.traject")}: 
      calls the parent method (\code{prepare} for \code{ADEg.Tr}) and modifies some graphical parameters used by default.}
      
    \item{panel}{\code{signature(object = "Tr.traject")}: 
      draws arrows, labels and points.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEg.Tr}}
  \code{\link{triangle.traject}}
}

\examples{
showClass("Tr.traject")
}

\keyword{classes}
