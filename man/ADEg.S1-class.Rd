\name{ADEg.S1-class}
\docType{class}

\alias{ADEg.S1}
\alias{ADEg.S1-class}

\alias{prepare,ADEg.S1-method}
\alias{setlatticecall,ADEg.S1-method}
\alias{gettrellis,ADEg.S1-method}
\alias{panelbase,ADEg.S1-method}

\title{Class \code{ADEg.S1}}

\description{
  An object of \code{ADEg.S1} class represents unidimensional data into one dimension.
  
  The \code{ADEg.S1} class is a virtual class, i.e. a class which is not possible to create objects but which have heirs.
  This class inherits from \code{ADEg} class and 
  has five son classes : \code{S1.boxplot}, \code{S1.class}, \code{S1.distri}, \code{S1.label} and \code{S1.match}. 
}

\section{Objects from the Class}{
  None object of this class can be instantiated.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{score}: the displayed values in the form of a numeric vector, a name or a matching call.}
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
      
    \item{\code{lattice.call}}{a list of two elements to create the \code{trellis} object: 
      \itemize{
        \item{\code{graphictype}: \code{xyplot}}
        \item{\code{arguments}: its parameters to obtain the \code{trellis} object} 
    }}
    
    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.S1} class.}
    
    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters:
     \itemize{
        \item{\code{hori.update}: a logical indicating if the sense of direction of the graphics is updating}
        \item{\code{backgrid}: a list of two elements for grid lines. \code{backgrid$x} defines the coordinates 
          of the lines (horizontal or vertical depending on the graphics orientation) and \code{backgrid$d} the grid mesh}
        \item{\code{rug}: an index value indicating where the rugs are drawn}
    }}
    
    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{ADEg}}, directly.
}

\section{Methods}{
  \describe{
    \item{prepare}{\code{signature(object = "ADEg.S1")}: 
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}
    
    \item{setlatticecall}{\code{signature(object = "ADEg.S1")}: 
      prepares the \code{lattice.call} slot}
      
    \item{panelbase}{\code{signature(object = "ADEg.S1")}: 
      defines the graphical background (e.g. grid, rugs and box)}
      
    \item{gettrellis}{\code{signature(object = "ADEg.S1")}: 
      converts the graphic into a \code{trellis} object of \code{lattice} class}
      
    \item{zoom}{\code{signature(object = "ADEg.S1", zoom = "numeric", center = "missing")}: 
      performs a zoom in (if zoom < 1) or out (if zoom > 1) centered, only in one-dimension}
      
    \item{zoom}{\code{signature(object = "ADEg.S1", zoom = "numeric", center = "numeric")}: 
      performs a zoom in (if zoom < 1) or out (if zoom > 1) around the center passed in parameter, only in one-dimension}
  }
}

\note{
  Various graphical parameters are used for display an ADEg.S1 object. 
  The list \code{p1d} in \code{adegpar()} is thought specific for \code{ADEg.S1} objects.

  The \code{ADEg.S1} class and \code{ADEg.C1} class are both used to represent an unidimensional information (e.g. a score).
  The difference between these two classes is mainly ideological : an \code{ADEg.S1} object is a representation into one 
  dimension (e.g. one line) while an \code{ADEg.C1} object is a representation into two dimensions (e.g. curves).
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link{adegpar}}
  \code{\link{zoom}}
  \code{\linkS4class{S1.boxplot}}
  \code{\linkS4class{S1.class}}
  \code{\linkS4class{S1.distri}}
  \code{\linkS4class{S1.label}}
  \code{\linkS4class{S1.match}}
  \code{\linkS4class{ADEg}}  
}

\examples{
showClass("ADEg.S1")
adegpar("p1d")
}

\keyword{classes}
