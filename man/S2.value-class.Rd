\name{S2.value-class}
\docType{class}

\alias{S2.value}
\alias{S2.value-class}
\alias{prepare,S2.value-method}
\alias{panel,S2.value-method}

\title{Class \code{S2.value}}

\description{
  A class for the creation and display of bi-dimensional plot with a third value represented (as a variable) by symbols.
}

\section{Objects from the Class}{
  \code{S2.value} objects can be created by calls of the form \code{new("S2.value", ...)}.
  
  The regular usage in this package is to use the \code{s.value} function.
}

\section{Slots}{
  \describe{
    \item{\code{data}:}{a list containing data or data's name. \itemize{
      \item{\code{dfxy}: the displayed values in the form of a data frame, a name or a matching call.}
      \item{\code{z}: a vector (or a matrix) with as many values as rows in \code{dfxy}.}
      \item{\code{xax}: an integer or a vector indicating the columns of \code{dfxy} kept for the x-axes.}
      \item{\code{yax}: an integer or a vector indicating the columns of \code{dfxy} kept for the y-axes.}
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
      The specific slots for \code{S2.value} objects are: \itemize{
        \item{\code{method}: the method of representation for \code{z} (color shading or proportional size).}
        \item{\code{symbol}: the type of symbol (square or circle).}
        \item{\code{center}: a center value for method \code{size}.}
        \item{\code{centerpar}: a logical or a list to represent center value using elements in the 
          \code{adegpar("ppoints")} list.}
        \item{\code{breaks}: a vector containing the breaks used for splitting \code{z} value. If \code{NULL}, \code{pretty(z, n)} is used.}
        \item{\code{nclass}: an integer for the number of desired intervals, ignored if \code{breaks} is not missing.}
        \item{\code{col}: a \code{NULL} value, a color or a colors vector to color symbols.}
     
    }}

    \item{\code{stats}}{a list of internal preliminary calculations}

    \item{\code{s.misc}}{a list of some others internal parameters:
      \itemize{
        \item{\code{plegend.update}: a logical indicating if the legend parameters are updating}
        \item{\code{breaks.update}: a logical indicating if the legend breaks are updating}
        \item{\code{lim.update}: a logical indicating if the limits are updating}
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
  The specific methods for \code{S2.value} are: 
  \describe{
    \item{prepare}{\code{signature(object = "S2.value")}: 
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and calculates limits.}
      
    \item{panel}{\code{signature(object = "S2.value")}: 
      draws symbols.}
  }
}

\note{
  For the symbol size, if the method is \code{size}, we use perceptual scaling (Tanimura et al. 2006).  
}

\references{
  Tanimura, S. and Kuroiwa, C. and Mizota, T. 2006. Proportional symbol
  mapping in R. \emph{Journal of Statistical Software}. \bold{15}, 1--7
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}    
  \code{\linkS4class{ADEg.S2}}
  \code{\link{s.value}}
}

\examples{
showClass("S2.value")
}

\keyword{classes}
