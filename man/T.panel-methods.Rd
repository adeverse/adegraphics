\name{T.panel-methods}
\docType{methods}

\alias{T.panel-methods}
\alias{T.panel}
\alias{T.panelbase}
\alias{T.panelbase-methods}
\alias{T.panelbase, ADEg.T-methods}

\title{Methods \code{T.panel} and \code{T.panelbase}}

\description{
  The methods \code{T.panel} and \code{T.panelbase} performs the display of the \code{ADEg.T} object.
}

\section{Methods}{
  \describe{
    \item{T.panelbase}{This method is a base method and has the same implementation for all \code{ADEg.T} objects. \itemize{
      \item{\code{signature(object = "ADEg.T")}}{
        draws the common background of all \code{ADEg.T} objects as axes, labels, ticks, box and grid.}
    }}
    
    \item{T.panel}{These methods are specifics for each \code{ADEg.T} sub-class.\itemize{
      \item{\code{signature(object = "T.cont")}}{
        draws mean points and regression lines.}
      \item{\code{signature(object = "T.image")}}{
        draws raster image.}
      \item{\code{signature(object = "T.value")}}{
        draws symbols.}
    }}
}}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\keyword{methods}

