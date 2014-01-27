\name{S2.panel-methods}

\docType{methods}

\alias{S2.panel-methods}
\alias{S2.panel}
\alias{S2.panelbase}
\alias{S2.panelbase-methods}
\alias{S2.panelbase,ADEg.S2-methods}

\title{Methods \code{S2.panel} and \code{S2.panelbase}}

\description{
  The methods \code{S2.panel} and \code{S2.panelbase} performs the display of the \code{ADEg.S2} object.
}

\section{Methods}{
  \describe{
    \item{S2.panelbase}{This method is a base method and has the same implementation for all \code{ADEg.S2} objects. \itemize{
      \item{\code{signature(object = "ADEg.S2")}: 
        draws the common background of all \code{ADEg.S2} objects as grid and box.}
    }}
    
    \item{S2.panel}{These methods are specifics for each \code{ADEg.S2} sub-class. \itemize{
      \item{\code{signature(object = "S2.arrow")}:
        draws points, arrows and labels.}
      \item{\code{signature(object = "S2.class")}:
        draws ellipses, convex hulls, stars, labels and points.}
      \item{\code{signature(object = "S2.corcircle")}:
        draws arrows, labels and axes.}
      \item{\code{signature(object = "S2.density")}:
        draws densities and external points.}
      \item{\code{signature(object = "S2.distri")}:
        draws ellipses, stars, labels and points.}
    	\item{\code{signature(object = "S2.image")}:
        draws raster image.}
    	\item{\code{signature(object = "S2.label")}:
        draws points and labels.}
      \item{\code{signature(object = "S2.logo")}:
        displays the logos.}
    	\item{\code{signature(object = "S2.match")}:
    	  draws arrows and labels.}
      \item{\code{signature(object = "S2.traject")}:
        draws points, arrows and labels.}
    	\item{\code{signature(object = "S2.value")}:
    	  draws symbols.)}
    }}
}}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\keyword{methods}
