\name{C1.panel-methods}

\docType{methods}

\alias{C1.panel}
\alias{C1.panel-methods}
\alias{C1.panelbase}
\alias{C1.panelbase-methods}
\alias{C1.panelbase,ADEg.C1-methods} 

\title{Methods \code{C1.panel} and \code{C1.panelbase}}

\description{
  The methods \code{C1.panel} and \code{C1.panelbase} performs the display of the \code{ADEg.C1} object.
}

\section{Methods}{
  \describe{
    \item{C1.panelbase}{This method is a base method and has the same implementation for all \code{ADEg.C1} objects. \itemize{
      \item{\code{signature(object = "ADEg.C1")}: 
        draws the common background of all \code{ADEg.C1} objects as grid, rugs and box.}
    }}

    \item{C1.panel}{These methods are specifics for each \code{ADEg.C1} sub-class. \itemize{
      \item{\code{signature(object = "C1.barchart")}:
        draws bar chart and labels.}
      \item{\code{signature(object = "C1.density")}:
        draws density curves.}
      \item{\code{signature(object = "C1.gauss")}:
        draws Gauss curves and level names of each curve.}
    }}
}}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\keyword{methods}
