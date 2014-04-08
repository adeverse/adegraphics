\name{T.image-class}
\docType{class}

\alias{T.image}
\alias{T.image-class}
\alias{prepare,T.image-method}
\alias{panel,T.image-method}

\title{Class \code{T.image}}

\description{
  A class for the representation of a matrix or table object in which values have different colors.
}

\section{Objects from the Class}{
  \code{T.image} objects can be created by calls of the form \code{new("T.image", ...)}.
  
  The regular usage in this package is to use the \code{table.image} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{dftab}: the displayed values which can be \code{table}, \code{dist} or \code{matrix}
        in the form of a data frame, a name or a matching call}
      \item{\code{coordsx}: an integer or a vector indicating the columns of \code{dftab} kept}
      \item{\code{coordsy}: an integer or a vector indicating the rows of \code{dftab} kept}
      \item{\code{labelsx}: columns labels}
      \item{\code{labelsy}: rows labels}
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

    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.T} class.
      The specific slots for \code{T.image} objects are: \itemize{
      \item{\code{breaks}: a vector of values to split \code{dftab}. If \code{NULL}, \code{pretty(dftab, nclass)} is used.}
      \item{\code{nclass}: an integer for the number of desired intervals, ignored if \code{breaks} is not missing.}
      \item{\code{col}: a \code{NULL} value, a color or a colors vector used for the cells.}
    }}

    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters:
      \itemize{
        \item{\code{breaks.update}: a logical indicating if the legend breaks is updating.}
    }}

    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{ADEg.T}}, directly.\cr
  Class \code{\linkS4class{ADEg}}, by class \code{ADEg.T}, distance 2.\cr
  Class \code{\linkS4class{ADEgORtrellis}}, by class \code{ADEg.T}, distance 3.\cr
  Class \code{\linkS4class{ADEgORADEgSORtrellis}}, by class \code{ADEg.T}, distance 3.
}

\section{Methods}{
  The methods of the father classes \code{"ADEg.T"} and \code{"ADEg"} can be used by inheritance.
  The specific methods for \code{T.image} are: 
  \describe{
    \item{prepare}{\code{signature(object = "T.image")}: 
      calls the parent method (\code{prepare} for \code{ADEg.T}) and modifies some graphical parameters used by default and calculates limits and grid.}
    
    \item{panel}{\code{signature(object = "T.image")}: 
      draws raster image.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.T}}
  \code{\link{table.image}}
}

\examples{
showClass("T.image")
}

\keyword{classes}
