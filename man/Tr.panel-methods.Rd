\name{Tr.panel-methods}

\docType{methods}

\alias{Tr.panel}
\alias{Tr.panel-methods}
\alias{Tr.panelbase}
\alias{Tr.panelbase-methods}
\alias{Tr.panelbase, ADEg.Tr-methods} 

\title{Methods \code{Tr.panel} and \code{Tr.panelbase}}

\description{
  The methods \code{Tr.panel} and \code{Tr.panelbase} performs the display of the \code{ADEg.Tr} object.
}

\section{Methods}{
  \describe{
    \item{Tr.panelbase}{This method is a base method and has the same implementation for all \code{ADEg.Tr} objects. \itemize{
      \item{\code{signature(object = "ADEg.Tr")}: 
        draws the common background of all \code{ADEg.Tr} objects as triangle and grid.}
    }}

    \item{Tr.panel}{These methods are specifics for each \code{ADEg.Tr} sub-class. \itemize{
      \item{\code{signature(object = "Tr.class")}: 
        draws arrows, labels and points.}
      \item{\code{signature(object = "Tr.label")}:
        draws lines, labels and points.}
      \item{\code{signature(object = "Tr.match")}:
        draws arrows, labels and points.}
    }}
}}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\keyword{methods}
