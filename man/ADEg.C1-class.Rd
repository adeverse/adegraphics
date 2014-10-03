\name{ADEg.C1-class}
\docType{class}

\alias{ADEg.C1}
\alias{ADEg.C1-class}

\alias{panelbase,ADEg.C1-method}
\alias{gettrellis,ADEg.C1-method}
\alias{prepare,ADEg.C1-method}
\alias{setlatticecall,ADEg.C1-method}

\title{Class \code{ADEg.C1}
}

\description{
  An object of \code{ADEg.C1} class represents unidimensional data into two dimensions.
  
  The \code{ADEg.C1} class is a virtual class, i.e. a class which is not possible to create objects but which have heirs.
  This class inherits from \code{ADEg} class and 
  has three son classes : \code{C1.barchart}, \code{C1.curve}, \code{C1.density}, \code{C1.dotplot}, \code{C1.gauss}, 
  \code{C1.hist}, \code{C1.interval}
}

\section{Objects from the Class}{
  None object of this class can be instantiated.
}

\section{Slots}{
  \describe{
    \item{\code{data}}{a list containing data or data's name. \itemize{
      \item{\code{score}: the displayed values in the form of a numeric vector, a name or a matching call.}
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
    
    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.C1} class}
    
    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters:
     \itemize{
        \item{\code{hori.update}: a logical indicating if the sense of direction of the graphics is updating}
        \item{\code{backgrid}: a list of two elements for grid lines. \code{backgrid$x} defines the coordinates 
          of the lines (horizontal or vertical depending on the graphics orientation) and \code{backgrid$d} the grid mesh}
    }}
    
    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{ADEg}}, directly.
}

\section{Methods}{
  \describe{
    \item{prepare}{\code{signature(object = "ADEg.C1")}: 
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}
    
    \item{setlatticecall}{\code{signature(object = "ADEg.C1")}: 
      prepares the \code{lattice.call} slot}
    
    \item{panelbase}{\code{signature(object = "ADEg.C1")}: 
      defines the graphical background (e.g. grid, rugs and box)}
    
    \item{gettrellis}{\code{signature(object = "ADEg.C1")}: 
      converts the graphic into a \code{trellis} object of \code{lattice} class}
   }
}

\note{
  The \code{ADEg.S1} class and \code{ADEg.C1} class are both used to represent an unidimensional information (e.g. a score).
  The difference between these two classes is mainly ideological : an \code{ADEg.S1} object is a representation into one 
  dimension (e.g. one line) while an \code{ADEg.C1} object is a representation into two dimensions (e.g. curves).
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link{adegpar}}
  \code{\linkS4class{C1.barchart}}
  \code{\linkS4class{C1.curve}}
  \code{\linkS4class{C1.density}}
  \code{\linkS4class{C1.dotplot}}
  \code{\linkS4class{C1.gauss}}
  \code{\linkS4class{C1.hist}}
  \code{\linkS4class{C1.interval}}
  \code{\linkS4class{ADEg}}
}

\examples{
showClass("ADEg.C1")
}

\keyword{classes}
