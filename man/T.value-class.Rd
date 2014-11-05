\name{T.value-class}
\docType{class}

\alias{T.value}
\alias{T.value-class}
\alias{prepare,T.value-method}
\alias{panel,T.value-method}

\title{Class \code{T.value}}

\description{
  A class for the representation of a matrix, a data frame, or a distance matrix using symbols, varying in size or color.
}

\section{Objects from the Class}{
  \code{T.value} objects can be created by calls of the form \code{new("T.value", ...)}.
  
  The regular usage in this package is to use the \code{table.value} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}:}{a list containing data or data's name. \itemize{
      \item{\code{dftab}: the displayed values which can be \code{table}, \code{dist} or \code{matrix}
        in the form of a data frame, a name or a matching call}
      \item{\code{coordsx}: an integer or a vector indicating the columns of \code{dftab} kept}
      \item{\code{coordsy}: an integer or a vector indicating the rows of \code{dftab} kept}
      \item{\code{labelsx}: the columns' labels}
      \item{\code{labelsy}: the rows' labels}
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
      The specific slots for \code{T.value} objects are: \itemize{
      \item{\code{breaks}: a vector of values to split \code{dftab}. If \code{NULL}, \code{pretty(dftab, nclass)} is used.}
      \item{\code{nclass}: an integer for the number of desired intervals, ignored if \code{breaks} is not missing.}
      \item{\code{col}: a \code{NULL} value, a color or a colors vector to color symbols.}
      \item{\code{method}: the method of representation for \code{dftab} (color shading or proportional size).}
      \item{\code{symbol}: the type of symbol (square or circle).}
      \item{\code{center}: a center value for method \code{size}.}
      \item{\code{centerpar}: a logical or a list to represent center value using elements in the 
        \code{adegpar("ppoints")} list.}
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
  The specific methods for \code{T.value} are: 
  \describe{
    \item{prepare}{\code{signature(object = "T.value")}: 
      calls the parent method (\code{prepare} for \code{ADEg.T}) and modifies some graphical parameters used by default and calculates limits and grid.}
    
    \item{panel}{\code{signature(object = "T.value")}: 
      draws symbols.}
  }
}

\note{
  For the symbol size, if the method is \code{size}, we use perceptual scaling (Tanimura et al. 2006) .  
}

\references{
  Tanimura, S. and Kuroiwa, C. and Mizota, T. 2006 Proportional symbol
  mapping in R \emph{Journal of Statistical Software} \bold{15}, 1--7 
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.T}}
  \code{\linkS4class{T.cont}}
  \code{\link{table.value}}
}

\examples{
showClass("T.value")
}

\keyword{classes}
