\name{ADEgS-class}
\docType{class}

\alias{ADEgS-class}

\alias{$,ADEgS-method}
\alias{[,ADEgS,numeric,missing,logical-method}
\alias{[,ADEgS,numeric,missing,missing-method}
\alias{[[,ADEgS,numeric,missing-method}
\alias{[[,ADEgS,character,missing-method}
\alias{[[<-,ADEgS,numeric,missing,ADEg-method}
\alias{[[<-,ADEgS,numeric,missing,ADEgS-method}  

\alias{getcall,ADEgS-method}
\alias{getgraphics}
\alias{getgraphics,ADEgS-method}
\alias{getpositions}
\alias{getpositions,ADEgS-method}
\alias{length,ADEgS-method}
\alias{names,ADEgS-method}
\alias{names<-,ADEgS,character-method}

\alias{show,ADEgS-method}
\alias{plot,ADEgS-method}
\alias{plot,ADEgS,ANY-method}
\alias{print,ADEgS-method}

\alias{update,ADEgS}
\alias{update,ADEgS-method}

\title{Class \code{"ADEgS"}}

\description{
  An object of \code{ADEgS} class is a complex graphic.
  This class allows the superposition, the insertion and/or the juxtaposition of several \code{ADEg}, \code{trellis} and/or \code{ADEgS} objects.
  
  The \code{ADEgS} class have neither father class nor son classes.
}

\section{Objects from the Class}{
  \code{ADEgS} objects can be created by calls of the form \code{new("ADEgS", ...)}.
  
  The regular usage in this package is to use the \code{ADEgS}, \code{add.ADEg}, \code{superpose}, \code{insert} or \code{+} functions.
}

\section{Slots}{
  \describe{
    \item{\code{ADEglist}}{a list of several \code{trellis}, \code{ADEg} and/or \code{ADEgS} objects.}
    
    \item{\code{positions}}{a matrix with four columns and as many rows as the number of graphical objects in the \code{ADEglist} slot.
      For each simple graphic, i.e. in each row, the coordinates of the top-right and the bottom-left hand corners are in \code{npc} unit (normalized parent coordinates).}
      
    \item{\code{add}}{a square matrix with as many rows and columns as the number of graphical objects in the \code{ADEglist} slot.
      The value at the i-th row and j-th column is equal to 1 whether the j-th graphical object in the \code{ADEglist} slot is superpose to i-th graphical one.
      Otherwise, this value is equal to 0.}
      
    \item{\code{Call}}{an object of class \code{call}}
  }
}

\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "ADEgS", i = "numeric", j = "missing", drop = "logical")}:
      extracts the \code{i}-th sub-graphics in the \code{x@ADEglist}. \code{i} can be a vector. 
      If \code{i} is a single number and if the extracted graphic in an \code{ADEg} object, the sub-selection 
      is in the form of \code{ADEg} if \code{drop} is \code{TRUE} and in the form of \code{ADEgS} otherwise.}
      
    \item{[}{\code{signature(x = "ADEgS", i = "numeric", j = "missing", drop = "missing")}: 
      the same than the previous method. \code{drop} is \code{FALSE} by default}
      
    \item{[[}{\code{signature(x = "ADEgS", i = "numeric", j = "missing")}: 
      extracts one sub-graphic, the \code{i}-th one, in the \code{x@ADEglist}}
      
    \item{[[}{\code{signature(x = "ADEgS", i = "character", j = "missing")}: 
      extracts one sub-graphic, named \code{i} in the \code{x@ADEglist}}
      
    \item{[[<-}{\code{signature(x = "ADEgS", i = "numeric", j = "missing", value = "ADEg")}: 
      replaces one sub graphic, the \code{i}-th one, by an \code{ADEg} object in the \code{x@ADEglist}}
      
    \item{[[<-}{\code{signature(x = "ADEgS", i = "numeric", j = "missing", value = "ADEgS")}: 
      replaces one sub graphic, the \code{i}-th one, by an \code{ADEgS} object in the \code{x@ADEglist}}
      
    \item{$}{\code{signature(x = "ADEgS")}: 
      extracts one sub-graphic by its name in the \code{x@ADEglist}}
      
    \item{getpositions}{\code{signature(object = "ADEgS")}: 
      returns the positions matrix of the object, i.e. \code{object@positions}}
      
    \item{getgraphics}{\code{signature(object = "ADEgS")}: 
      returns the list of graphics of the object, 
      i.e. \code{object@ADEglist}}
      
    \item{getcall}{\code{signature(object = "ADEgS")}: 
      returns the call of the object, i.e. \code{object@Call}}
      
    \item{names}{\code{signature(object = "ADEgS")}: 
      returns the graphics' names of the object, i.e. the names of \code{object@ADEglist}}
      
    \item{names<-}{\code{signature(object = "ADEgS")}: 
      replaces the graphics' names of the object, i.e. the names of \code{object@ADEglist}}
      
    \item{length}{\code{signature(x = "ADEgS")}: 
      returns the number of graphics into \code{x}, i.e. the length of \code{x@ADEglist}}
      
    \item{plot}{\code{signature(x = "ADEgS")}: same as \code{print}}
    
    \item{print}{\code{signature(x = "ADEgS")}: 
      displays the graphical elements into one device using positions and superposition management (\code{x@add} matrix)}
      
    \item{show}{\code{signature(object = "ADEgS")}: same as \code{print}} 
      
    \item{superpose}{\code{signature(g1 = "ADEgS", g2 = "ADEg", which = "numeric", plot = "logical")}: 
      creates a new \code{"ADEgS"} object performing a superposition of \code{g2} on the which-th \code{ADEg} object of \code{g1}. 
      This object is printed if \code{plot} is \code{TRUE}.}
      
    \item{superpose}{\code{signature(g1 = "ADEgS", g2 = "ADEg", which = "numeric", plot = "ANY")}: 
      creates a new \code{"ADEgS"} object performing a superposition of \code{g2} on the which-th \code{ADEg} object of \code{g1}. 
      This object is printed only if \code{plot} is \code{TRUE}.}
      
    \item{superpose}{\code{signature(g1 = "ADEgS", g2 = "ADEg", which = "missing", plot = "ANY")}: 
      creates a new \code{"ADEgS"} object performing a superposition of \code{g2} on the last \code{ADEg} object of \code{g1}. 
      This object is printed only if \code{plot} is \code{TRUE}.}
      
    \item{superpose}{\code{signature(g1 = "ADEgS", g2 = "ADEgS", which = "missing", plot = "ANY")}:
      creates a new \code{"ADEgS"} object performing a superposition between two \code{ADEgS} having the same length and the same \code{positions} slot. 
      It is used when \code{g1} and \code{g2} are both created with a partition of individual groups, variables or analysis' axis.}  
      
    \item{+}{\code{signature(e1 = "ADEg", e2 = "ADEgS")}:
      creates a new \code{"ADEgS"} object performing a superposition of \code{e1} on \code{e2}.}
    
    \item{+}{\code{signature(e1 = "ADEgS", e2 = "ADEg")}:
      creates a new \code{"ADEgS"} object performing a superposition of \code{e2} on \code{e1}.}
      
    \item{cbindADEg}{\code{signature(g1 = "ADEgORADEgS", g2 = "ADEgORADEgS")}:
      creates a new \code{"ADEgS"} object combining \code{g1} on \code{g2} by columns.}      
      
    \item{rbindADEg}{\code{signature(g1 = "ADEgORADEgS", g2 = "ADEgORADEgS")}:
      creates a new \code{"ADEgS"} object combining \code{g1} on \code{g2} by rows.}
      
    \item{update}{\code{signature(object = "ADEgS")}: 
      modifies the graphical parameters of each sub-graphics listed in \code{object@ADEglist} and/or the object's names 
      (with the key word \code{names}) and/or the \code{object@positions} slot (with the key word \code{positions}), after creation of the \code{object}.
      The current display is updated and a modified object is returned.}
      
    \item{insert}{\code{signature(graphics = "ADEgS", oldgraphics = "missing", posi, ratio, inset, plot, which, dispatch)}: 
      creates a new \code{"ADEgS"} object performing an insertion of \code{graphics} into the current device.}
      
    \item{insert}{\code{signature(graphics = "ADEgS", oldgraphics = "ADEg", posi, ratio, inset, plot)}: 
      creates a new \code{"ADEgS"} object performing an insertion of \code{graphics} into \code{oldgraphics}.}
      
    \item{insert}{\code{signature(graphics = "ADEgORtrellis", oldgraphics = "ADEgS", posi, ratio, inset, plot, which)}: 
      creates a new \code{"ADEgS"} object performing an insertion of \code{graphics} into \code{oldgraphics}.}
      
    \item{insert}{\code{signature(graphics = "ADEgS", oldgraphics = "ADEgS", posi, ratio, inset, plot, which, dispatch)}: 
      creates a new \code{"ADEgS"} object performing an insertion of \code{graphics} into \code{oldgraphics}.}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\seealso{
  \code{\linkS4class{ADEg}}
  \code{\link{ADEgS}}
  \code{\link{superpose}}
  \code{\link{insert}}
}

\examples{
  showClass("ADEgS")
}

\keyword{classes}
