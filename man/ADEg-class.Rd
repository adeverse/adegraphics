\name{ADEg-class}
\docType{class}

\alias{ADEg}
\alias{ADEg-class}

\alias{add.ADEg,ADEg-method}
\alias{getcall,ADEg-method}
\alias{getlatticecall}
\alias{getlatticecall,ADEg-method}
\alias{getparameters}
\alias{getparameters,ADEg-method}
\alias{getstats}
\alias{getstats-methods}
\alias{getstats,ADEg-method}
\alias{gettrellis}
\alias{gettrellis-methods}
\alias{gettrellis,ADEg-method}
\alias{panelbase}
\alias{panelbase-methods}
\alias{panelbase,ADEg-method}
\alias{printSuperpose,ADEgORtrellis,ADEgORtrellis-method}
\alias{plot,ADEg-method}
\alias{plot,ADEg,ANY-method}
\alias{print,ADEg-method}
\alias{show,ADEg-method}
\alias{update,ADEg}
\alias{update,ADEg-method}

\title{Class \code{ADEg}
}

\description{
  An object of \code{ADEg} class is a simple graphic.
  This object can be blended in with another one (superposition, insertion and/or juxtaposition) to 
  form a more complex graphics (an \code{ADEgS} object).
  
  The \code{ADEg} class is a virtual class, i.e. a class which is not possible to create objects but which have heirs.
  This class has five son classes : \code{ADEg.S1}, \code{ADEg.S2}, \code{ADEg.C1}, \code{ADEg.T} and \code{ADEg.Tr}.
}

\section{Objects from the Class}{
  None object of this class can be instantiated.
}

\section{Slots}{
  \describe{
    \item{\code{trellis.par}}{a list of parameters for \code{lattice} call. 
      It will be passed directly to \code{par.settings} arguments of the \code{lattice} function.}
      
    \item{\code{adeg.par}}{a list of graphical parameters, corresponding to the ones 
      given by \code{adegpar()} function.}
      
    \item{\code{lattice.call}}{a list of two elements to create the \code{trellis} object: 
      \itemize{
        \item{\code{graphictype}: the \code{lattice} function to use}
        \item{\code{arguments}: its parameters to obtain the \code{trellis} object} 
    }}
    
    \item{\code{g.args}}{a list containing some parameters linked with the created object of \code{ADEg} class:
      \itemize{
        \item{\code{xlim}, \code{ylim}}
        \item{\code{main}, \code{sub}}
        \item{\code{xlab}, \code{ylab}}
        \item{\code{samelimits}}
        \item{\code{scales}: a list of scales informations (ticks, marks and labels for the x-axis or the y-axis) 
          in the form of the lattice argument \code{scales} in the \code{xyplot} function}
    }}
      
    \item{\code{stats}}{a list of internal preliminary calculations}
    
    \item{\code{s.misc}}{a list of some other internal parameters}
    
    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Methods}{
  \describe{
    \item{panelbase}{\code{signature(object = "ADEg")}: draws grid and text and produces graphical output 
      from the graphical object}
    
    \item{getcall}{\code{signature(object = "ADEg")}: returns the \code{Call} slot}
    
    \item{getlatticecall}{\code{signature(object = "ADEg")}: returns the \code{lattice.call} slot}
    
    \item{getstats}{\code{signature(object = "ADEg")}: returns the \code{stats} slot}
    
    \item{getparameters}{\code{signature(object = "ADEg", number)}:
      if \code{number} is 1, returns the \code{trellis.par} slot,  
      if it is 2, returns the \code{adeg.par} slot
      and if it is 0, returns the both slots}
    
    \item{add.ADEg}{\code{signature(object = "ADEg")}: superposes an ADEg on the 
      current one plotted}
      
    \item{+}{\code{signature(e1 = "ADEg", e2 = "ADEg")}: superposes e2 on e1}
    
    \item{superpose}{\code{signature(g1 = "ADEgORtrellis", g2 = "ADEgORtrellis", which = "ANY", plot = "ANY")}: 
      creates a new \code{ADEgS} object performing a superposition of \code{g2} on \code{g1}.}
      
    \item{printSuperpose}{\code{signature(g1 = "ADEgORtrellis", refg = "ADEgORtrellis")}: 
      internal method, not for users.}
      
    \item{cbindADEg}{\code{signature(g1 = "ADEgORADEgS", g2 = "ADEgORADEgS")}:
      creates a new \code{"ADEgS"} object combining \code{g1} on \code{g2}.}
      
    \item{rbindADEg}{\code{signature(g1 = "ADEgORADEgS", g2 = "ADEgORADEgS")}:
      creates a new \code{"ADEgS"} object combining \code{g1} on \code{g2} by rows.}
      
    \item{insert}{\code{signature(graphics = "ADEgORtrellis", oldgraphics = "missing", posi, ratio, inset, plot, which)}: 
      creates a new \code{ADEgS} object performing an insertion of \code{graphics} into the current device.}
      
    \item{insert}{\code{signature(graphics = "ADEgORtrellis", oldgraphics = "ADEg", posi, ratio, inset, plot)}: 
      creates a new \code{ADEgS} object performing an insertion of \code{graphics} into \code{oldgraphics}.}      
          
    \item{show}{\code{signature(x = "ADEg")}: prints the \code{ADEg} object}
    
    \item{plot}{\code{signature(x = "ADEg")}: prints the \code{ADEg} object}
    
    \item{print}{\code{signature(x = "ADEg")}: displays the \code{ADEg} object
      in the current device or in a new one}
     
    \item{update}{\code{signature(object = "ADEg")}: modifies graphical parameters after 
      the \code{ADEg} creation, updates the current display and returns the modified \code{ADEg}}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\note{
  For any \code{ADEg} creation, various graphical parameters can be passed into the dots (...) arguments.
  \itemize{
    \item{the parameters listed in \code{adegpar()} can be changed, even if
      some of them do no modify the graphic representation chosen.}
  
    \item{the \code{lattice} parameters listed in \code{trellis.par.get()} can also be changed.}
    
    \item{limits, main and sub title, and axes labels can be changed using the keys 
      \code{xlim}, \code{ylim}, \code{main}, \code{sub}, \code{xlab} and \code{ylab}.}
    
    \item{a neighbouring graph (object of class \code{nb} or \code{listw}) and a spatial one (object of
      class \code{sp}) can be display in the background using the keys \code{nbobject}, \code{Sp} and \code{sp.layout}.}
  }
}

\seealso{
  \code{\linkS4class{ADEgS}}
  \code{\link{adegpar}}
  \code{\link{superpose}}
  \code{\link{insert}}
}

\examples{
showClass("ADEg")
}

\keyword{classes}
