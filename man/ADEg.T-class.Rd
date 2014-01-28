\name{ADEg.T-class}
\docType{class}

\alias{ADEg.T}
\alias{ADEg.T-class}

\alias{gettrellis,ADEg.T-method}
\alias{prepare,ADEg.T-method}
\alias{setlatticecall,ADEg.T-method}
\alias{panelbase,ADEg.T-method}

\title{Class \code{ADEg.T}}

\description{
  An object of \code{ADEg.T} class represents table data.
  
  The \code{ADEg.T} class is a virtual class, i.e. a class which is not possible to create objects but which have heirs.
  This class inherits from \code{ADEg} class and 
  has two son classes : \code{T.image} and \code{T.value}.  
}

\section{Objects from the Class}{
  None object of this class can be instantiated.
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
      \item{"\code{frame}: a positive or null integer. It is the number of the frame containing the data (used 
        with \code{sys.frame(..., env = data$frame)}). Only if the data are not stored (i.e. \code{data$storeData = FALSE}).}
      \item{\code{storeData}: a logical indicating if the data should be stored in the returned object. 
        If \code{FALSE}, only the names of the data arguments are stored.}
    }}
    
    \item{\code{trellis.par}}{a list of parameters for \code{lattice} call. 
      It will be passed directly to \code{par.settings} arguments of the \code{lattice} function.}
  
    \item{\code{adeg.par}}{a list of graphical parameters, corresponding to the ones 
      given by \code{adegpar()} function.}
      
    \item{\code{lattice.call}}{a list of two elements to create the \code{trellis} object: 
      \itemize{
        \item{\code{graphictype}: \code{xyplot}}
        \item{\code{arguments}: its parameters to obtain the \code{trellis} object} 
    }}
    
    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.T} class:
      \itemize{
        \item{\code{method}: only for \code{T.value} objects}
        \item{\code{symbol}: only for \code{T.value} objects}
        \item{\code{center}: only for \code{T.value} objects}
    }}
    
    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters:
      \itemize{
        \item{\code{breaks.update}: a logical indicating if the legend breaks is updating}
        \item{\code{axes$dx} and \code{axes$dy}: intervals for the cell size}
    }}
    
    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{ADEg}}, directly.
}

\section{Methods}{
  \describe{
    \item{prepare}{\code{signature(object = "ADEg.T")}: 
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}
    
    \item{setlatticecall}{\code{signature(object = "ADEg.T")}: 
      prepares the \code{lattice.call} slot}
    
    \item{panelbase}{\code{signature(object = "ADEg.T")}: 
      defines the graphical background (e.g. axes, labels, ticks, box and grid)}
    
    \item{gettrellis}{\code{signature(object = "ADEg.T")}: 
      converts the graphic into a \code{trellis} object of \code{lattice} class}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link{adegpar}}
  \code{\linkS4class{T.image}}
  \code{\linkS4class{T.value}}
  \code{\linkS4class{ADEg}}  
}

\examples{
showClass("ADEg.T")
}

\keyword{classes}
