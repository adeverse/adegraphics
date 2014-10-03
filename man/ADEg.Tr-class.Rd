\name{ADEg.Tr-class}
\docType{class}

\alias{ADEg.Tr}
\alias{ADEg.Tr-class}

\alias{gettrellis,ADEg.Tr-method}
\alias{prepare,ADEg.Tr-method}
\alias{setlatticecall,ADEg.Tr-method}
\alias{panelbase,ADEg.Tr-method}

\title{Class \code{"ADEg.Tr"}}

\description{ 
  An object of \code{ADEg.Tr} class represents triangular coordinates in 2D.
  
  The \code{ADEg.Tr} class is a virtual class, i.e. a class which is not possible to create objects but which have heirs.
  This class inherits from \code{ADEg} class and 
  has three son classes : \code{Tr.class}, \code{Tr.label}, \code{T.match} and \code{T.traject}.  
}

\section{Objects from the Class}{
  None object of this class can be instantiated.
}

\section{Slots}{
  \describe{
    \item{\code{data}:}{a list containing data or data's name. \itemize{
      \item{\code{dfxyz}: the displayed values in the form of a data frame with three columns, a name or a matching call.}
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

    \item{\code{g.args}}{a list containing some method parameters linked with the created object of \code{ADEg.Tr} class:
      \itemize{
      \item{\code{max3d} and \code{min3d}: triangular limits}
      \item{\code{adjust}: a logical to adjust the device with the limits}
    }}
    
    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some others internal parameters:
     \itemize{
        \item{\code{adjust.update}: a logical indicating if the \code{adjust} slot is updating}
        \item{\code{cornerp}: coordinates of the triangle extremities.}
        \item{\code{lgrid}: a list containing the three coordinates of the grid segments extremities(\code{pts1}, 
          \code{pts2}, \code{pts3}) and the value of the division (\code{posgrid})}
    }}
    
    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Extends}{
  Class \code{\linkS4class{ADEg}}, directly.
}

\section{Methods}{
  \describe{
    \item{prepare}{\code{signature(object = "ADEg.Tr")}: 
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}

    \item{setlatticecall}{\code{signature(object = "ADEg.Tr")}: 
      prepares the \code{lattice.call} slot}
      
    \item{panelbase}{\code{signature(object = "ADEg.Tr")}: 
      defines the graphical background (e.g. triangle and grid)}
      
    \item{gettrellis}{\code{signature(object = "ADEg.Tr")}: 
      converts the graphic into a \code{trellis} object of \code{lattice} class}
  }
}
\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\link{adegpar}}
  \code{\linkS4class{Tr.class}}
  \code{\linkS4class{Tr.label}}
  \code{\linkS4class{Tr.match}}
  \code{\linkS4class{Tr.traject}}
  \code{\linkS4class{ADEg}}  
}

\examples{
showClass("ADEg.Tr")
}

\keyword{classes}
