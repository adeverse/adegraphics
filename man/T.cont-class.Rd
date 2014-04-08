\name{T.cont-class}
\docType{class}

\alias{T.cont}
\alias{T.cont-class}
\alias{panel,T.cont-method}

\title{Class \code{T.cont}}

\description{
  A class for the representation of a contingency table object with statistical information (mean and regression lines).
}

\section{Objects from the Class}{
  \code{T.cont} objects can be created by calls of the form \code{new("T.cont", ...)}.
  
  The regular usage in this package is to use the \code{table.value} function with a \code{table} object.
}

\section{Slots}{
  \describe{
    \item{\code{data}:}{a list containing data or data's name. \itemize{
      \item{\code{dftab}: a contingency table object in the form of a data frame, a name or a matching call}
      \item{\code{coordsx}: an integer or a vector indicating the columns of \code{dftab} kept}
      \item{\code{coordsy}: an integer or a vector indicating the rows of \code{dftab} kept}
      \item{\code{labelsx}: the columns' labels}
      \item{\code{labelsy}: the rows' labels}
      \item{"\code{frame}: a positive or null integer. It is the number of the frame containing the data (used 
        with \code{sys.frame(..., env = data$frame)}). Only if the data are not stored (i.e. \code{data$storeData = FALSE}).}
      \item{\code{storeData}: a logical indicating if the data should be stored in the returned object. 
        If \code{FALSE}, only the names of the data arguments are stored.}
    }}

    \item{\code{trellis.par}}{a list of parameters for \code{lattice} call. 
      It will be passed directly to \code{par.settings} arguments of the \code{lattice} function.}
  
    \item{\code{adeg.par}}{a list of graphical parameters, corresponding to the ones 
      given by \code{adegpar()} function.}
      
    \item{\code{lattice.call}}{a list to create the \code{trellis} object.}

    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{T.value} class.
      The specific slots for \code{T.cont} objects are: \itemize{
        \item{\code{meanX}: a logical to represent columns' means by points.}
        \item{\code{meanY}: a logical to represent rows' means by points.}
        \item{\code{ablineX}: a logical to represent columns' regression lines.}
        \item{\code{ablineY}: a logical to represent columns' regression lines.}
    }}
  
    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters}
    
    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{T.value}}, directly.\cr
  Class \code{\linkS4class{ADEg.T}}, by class \code{T.value}, distance 2.\cr
  Class \code{\linkS4class{ADEg}}, by class \code{T.value}, distance 3.\cr
  Class \code{\linkS4class{ADEgORtrellis}}, by class \code{T.value}, distance 4.\cr
  Class \code{\linkS4class{ADEgORADEgSORtrellis}}, by class \code{T.value}, distance 4.
}

\section{Methods}{
  The methods of the father classes \code{"T.value"}, \code{"ADEg.T"} and \code{"ADEg"} can be used by inheritance.
  The specific methods for \code{T.cont} are: 
  \describe{
    \item{panel}{\code{signature(object = "T.cont")}: 
      draws mean points and regression lines.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.T}}
  \code{\linkS4class{T.value}}
  \code{\link{table.value}}
}

\examples{
showClass("T.cont")
}

\keyword{classes}
