\name{S1.panel-methods}

\docType{methods}

\alias{S1.panel}
\alias{S1.panel-methods}
\alias{S1.panelbase}
\alias{S1.panelbase-methods}
\alias{S1.panelbase, ADEg.S1-methods} 

\title{Methods \code{S1.panel} and \code{S1.panelbase}}

\description{
  The methods \code{S1.panel} and \code{S1.panelbase} performs the display of the \code{ADEg.S1} object.
}

\section{Methods}{
  \describe{
    \item{S1.panelbase}{This method is a base method and has the same implementation for all \code{ADEg.S1} objects. \itemize{
      \item{\code{signature(object = "ADEg.S1")}: 
        draws the common background of all \code{ADEg.S1} objects as grid, rugs and box.}
    }}

    \item{S1.panel}{These methods are specifics for each \code{ADEg.S1} sub-class. \itemize{
      \item{\code{signature(object = "S1.boxplot")}: 
        draws box-and-wiskers diagrams, mean points and labels.}
      \item{\code{signature(object = "S1.class")}:
        draws labels and lines matching with score values.}
      \item{\code{signature(object = "S1.distri")}:
        draws mean points and segments with matching labels.}
      \item{\code{signature(object = "S1.label")}:
        draws labels and its links with score points.}
      \item{\code{signature(object = "S1.match")}:
        draws score points and matching segments and labels.}
    }}
}}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\keyword{methods}
