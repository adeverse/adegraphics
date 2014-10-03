\name{ADEg.S2-class}
\docType{class}

\alias{ADEg.S2}
\alias{ADEg.S2-class}

\alias{prepare,ADEg.S2-method}
\alias{setlatticecall,ADEg.S2-method}
\alias{gettrellis,ADEg.S2-method}
\alias{panelbase,ADEg.S2-method}

\title{Class \code{ADEg.S2}}

\description{
  An object of \code{ADEg.S2} class represents bi-dimensional data.
  
  The \code{ADEg.S2} class is a virtual class, i.e. a class which is not possible to create objects but which have heirs.
  This class inherits from \code{ADEg} class and 
  has eleven son classes : \code{S2.arrow}, \code{S2.class}, \code{S2.corcircle}, \code{S2.density}, \code{S2.distri}, 
  \code{S2.image}, \code{S2.label}, \code{S2.logo}, \code{S2.match}, \code{S2.traject} and \code{S2.value}.  
}

\section{Objects from the Class}{
  None object of this class can be instantiated.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{dfxy}: the displayed values in the form of a data frame, a name or a matching call.}
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
      
    \item{\code{lattice.call}}{a list of two elements to create the \code{trellis} object: 
      \itemize{
        \item{\code{graphictype}: \code{xyplot}}
        \item{\code{arguments}: its parameters to obtain the \code{trellis} object} 
    }}
    
    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.S2} class:
      \itemize{
      \item{\code{fullcircle}: only for \code{S2.corcircle} objects}
      \item{\code{method}: only for \code{S2.value} objects}
      \item{\code{symbol}: only for \code{S2.value} objects}
      \item{\code{center}: only for \code{S2.value} objects}
    }}
    
    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters:
     \itemize{
        \item{\code{xfullcircle.update} and \code{yfullcircle.update}: a logical indicating if the circle size is updating (only for \code{S2.corcircle} objects)}
        \item{\code{plegend.update}: a logical indicating if the legend parameters are updating}
        \item{\code{breaks.update}: a logical indicating if the legend breaks are updating}
        \item{\code{backgrid}: a list of elements for grid lines}
    }}
    
    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{ADEg}}, directly.
}

\section{Methods}{
  \describe{
    \item{prepare}{\code{signature(object = "ADEg.S2")}: 
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}
    
    \item{setlatticecall}{\code{signature(object = "ADEg.S2")}: 
      prepares the \code{lattice.call} slot}
    
    \item{panelbase}{\code{signature(object = "ADEg.S2")}: 
      defines the graphical background (e.g. grid and box)}
  
    \item{gettrellis}{\code{signature(object = "ADEg.S2")}: 
      converts the graphic into a \code{trellis} object of \code{lattice} class}
      
    \item{zoom}{\code{signature(object = "ADEg.S2", zoom = "numeric", center = "missing")}: 
      performs a zoom in (if zoom < 1) or out (if zoom > 1) centered}
    
    \item{zoom}{\code{signature(object = "ADEg.S2", zoom = "numeric",	center = "numeric")}: 
      performs a zoom in (if zoom < 1) or out (if zoom > 1) around the center passed in parameter (center should be a two-length vector)}
    
    \item{addhist}{\code{signature(object = "ADEg.S2")}: 
      adds histograms and density lines against a bi-dimensional graphics}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link{addhist}}
  \code{\link{zoom}}
  \code{\link{adegpar}}
  \code{\linkS4class{S2.arrow}}
  \code{\linkS4class{S2.class}}
  \code{\linkS4class{S2.corcircle}}
  \code{\linkS4class{S2.density}}
  \code{\linkS4class{S2.distri}}
  \code{\linkS4class{S2.image}}
  \code{\linkS4class{S2.label}}
  \code{\linkS4class{S2.logo}}
  \code{\linkS4class{S2.match}}
  \code{\linkS4class{S2.traject}}
  \code{\linkS4class{S2.value}}
  \code{\linkS4class{ADEg}}
}

\examples{
showClass("ADEg.S2")
}

\keyword{classes}
