\name{S2.corcircle-class}
\docType{class}

\alias{S2.corcircle}
\alias{S2.corcircle-class}
\alias{prepare,S2.corcircle-method}
\alias{panel,S2.corcircle-method}

\title{Class \code{S2.corcircle}}

\description{
  A class for creating and drawing a correlation circle.
}

\section{Objects from the Class}{
  \code{S2.corcircle} objects can be created by calls of the form \code{new("S2.corcircle", ...)}.
  
  The regular usage in this package is to use the \code{s.corcircle} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{dfxy}: the displayed values in the form of a data frame, a name or a matching call.}
      \item{\code{xax}: an integer or a vector indicating the columns of \code{dfxy} kept for the x-axes.}
      \item{\code{yax}: an integer or a vector indicating the columns of \code{dfxy} kept for the y-axes.}
      \item{\code{labels}: a vector containing the points' labels.}
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
      The specific slot for \code{S2.corcircle} objects is: \itemize{
      \item{\code{fullcircle}: a logical to include the complete circle (limits are then c(-1, 1)).}
    }}
    
    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters:
      \itemize{
        \item{\code{backgrid}: a list of elements for grid lines}
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
  The specific methods for \code{S2.corcircle} are: 
  \describe{
    \item{prepare}{\code{signature(object = "S2.corcircle")}: 
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and prepares the drawn grid.}
      
    \item{panel}{\code{signature(object = "S2.corcircle")}: 
      draws arrows, labels and axes.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.S2}}
  \code{\link{s.corcircle}}
}

\examples{
showClass("S2.corcircle")
}

\keyword{classes}

