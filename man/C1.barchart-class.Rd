\name{C1.barchart-class}
\docType{class}

\alias{C1.barchart}
\alias{C1.barchart-class}
\alias{prepare,C1.barchart-method}
\alias{panel,C1.barchart-method}

\title{Class \code{C1.barchart}}

\description{
  A class for the creation and display of a numeric score using barcharts.
}

\section{Objects from the Class}{
  \code{C1.barchart} objects can be created by calls of the form \code{new("C1.barchart", ...)}.
  
  The regular usage in this package is to use the \code{s1d.barchart} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{score}: the displayed values in the form of a vector, a factor, a name or a matching call.}
      \item{\code{labels}: the labels' names drawn on the top of bars.}
      \item{\code{at}: the index value.}
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
      The specific slot for \code{C1.barchart} objects is: \itemize{
      \item{\code{sort}: a logical indicating if \code{score} is sorted in ascending order.}
    }}
    
    \item{\code{stats}}{a list of internal preliminary calculations}

    \item{\code{s.misc}}{a list of some others internal parameters}
    
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
  The specific methods for \code{C1.barchart} are: 
  \describe{
    \item{prepare}{\code{signature(object = "C1.barchart")}: 
      calls the parent method (\code{prepare} for \code{ADEg.C1}) and modifies some graphical parameters used by default.}
      
    \item{panel}{\code{signature(object = "C1.barchart")}: 
      draws bar charts and labels.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.C1}}
  \code{\link{s1d.barchart}}
}

\examples{
showClass("C1.barchart")
}

\keyword{classes}
