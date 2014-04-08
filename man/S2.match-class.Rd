\name{S2.match-class}
\docType{class}

\alias{S2.match}
\alias{S2.match-class}
\alias{prepare,S2.match-method}
\alias{panel,S2.match-method}

\title{Class \code{S2.match}}

\description{
  A class for the creation and display of paired coordinates in a bi-dimensional plot.
}

\section{Objects from the Class}{
  \code{S2.match} objects can be created by calls of the form \code{new("S2.match", ...)}.
  
  The regular usage in this package is to use the \code{s.match} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{dfxy}: the displayed values in the form of a data frame, a name or a matching call.}
      \item{\code{xax}: an integer or a vector indicating the columns of \code{dfxy} kept for the x-axes.}
      \item{\code{yax}: an integer or a vector indicating the columns of \code{dfxy} kept for the y-axes.}
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

    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.S2} class.
      The specific slot for \code{S2.match} objects is: \itemize{
        \item{\code{arrows}: a logical to draw arrows.}
    }}

    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters}
    
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
  The specific methods for \code{S2.match} are: 
  \describe{
    \item{prepare}{\code{signature(object = "S2.match")}: 
      calls the parent method (\code{prepare} for \code{ADEg.S2}) and modifies some graphical parameters used by default.}
      
    \item{panel}{\code{signature(object = "S2.match")}: 
      draws arrows and labels.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.S2}}
  \code{\link{s.match}}
}

\examples{
showClass("S2.match")
}

\keyword{classes}
