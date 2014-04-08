\name{Tr.match-class}
\docType{class}

\alias{Tr.match}
\alias{Tr.match-class}
\alias{prepare,Tr.match-method}
\alias{panel,Tr.match-method}

\title{Class \code{Tr.match}}

\description{
  A class for the creation and display of paired coordinates in a triangular plot.
}

\section{Objects from the Class}{
  \code{Tr.match} objects can be created by calls of the form \code{new("Tr.match", ...)}.
  
  The regular usage in this package is to use the \code{triangle.match} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{dfxyz}: the displayed values in the form of a three columns data frame, a name or a matching call.}
      \item{\code{labels}: a vector of character strings containing the matches' labels.}
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
      The specific slots for \code{Tr.match} objects are: \itemize{
        \item{\code{max3d} and \code{min3d}: vectors of three values for triangular maximal and minimal limits.}
        \item{\code{adjust}: a logical to adjust the device with the limits of the smaller equilateral triangle containing the values}
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
  The specific methods for \code{Tr.match} are: 
  \describe{
    \item{prepare}{\code{signature(object = "Tr.match")}: 
      calls the parent method (\code{prepare} for \code{ADEg.Tr}) and modifies some graphical parameters used by default.}
      
    \item{panel}{\code{signature(object = "Tr.match")}: 
      draws arrows, labels and points.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\linkS4class{ADEg.Tr}}
  \code{\link{triangle.match}}
}

\examples{
showClass("Tr.match")
}

\keyword{classes}
