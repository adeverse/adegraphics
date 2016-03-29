\name{panel-methods}
\docType{methods}

\alias{panel-methods}
\alias{panel}

\title{Methods \code{panel} for \code{ADEg} objects}

\description{
  The method \code{panel} displays all specific graphical components.
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "C1.barchart")}}{
      draws bar charts and labels}
    \item{\code{signature(object = "C1.curve")}}{
      draws points and curves}
    \item{\code{signature(object = "C1.curves")}}{
      draws multiple points and curves}
    \item{\code{signature(object = "C1.density")}}{
      draws density curves}
    \item{\code{signature(object = "C1.dotplot")}}{
      draws segments and dots}
    \item{\code{signature(object = "C1.gauss")}}{
      draws Gauss curves and level names of each curve}
    \item{\code{signature(object = "C1.hist")}}{
      draws rectangles}
    \item{\code{signature(object = "C1.interval")}}{ 
      draws segments or polygons}
    
    \item{\code{signature(object = "S1.boxplot")}}{
      draws box-and-wiskers diagrams, mean points and labels}
    \item{\code{signature(object = "S1.class")}}{
      draws labels and lines matching with score values}
    \item{\code{signature(object = "S1.distri")}}{
      draws mean points and segments with matching labels}
    \item{\code{signature(object = "S1.label")}}{
      draws labels and its links with score points}
    \item{\code{signature(object = "S1.match")}}{
      draws score points and matching segments and labels}
    
    \item{\code{signature(object = "S2.arrow")}}{
      draws points, arrows and labels}
    \item{\code{signature(object = "S2.class")}}{
      draws ellipses, convex hulls, stars, labels and points}
    \item{\code{signature(object = "S2.corcircle")}}{
      draws arrows, labels and axes}
    \item{\code{signature(object = "S2.density")}}{
      draws densities and external points}
    \item{\code{signature(object = "S2.distri")}}{
      draws ellipses, stars, labels and points}
    \item{\code{signature(object = "S2.image")}}{
      draws raster image}
    \item{\code{signature(object = "S2.label")}}{
      draws points and labels}
    \item{\code{signature(object = "S2.logo")}}{
      displays the logos}
    \item{\code{signature(object = "S2.match")}}{
      draws arrows and labels}
    \item{\code{signature(object = "S2.traject")}}{
      draws points, arrows and labels}
    \item{\code{signature(object = "S2.value")}}{
      draws symbols}
    
    \item{\code{signature(object = "T.cont")}}{
      draws mean points and regression lines}
    \item{\code{signature(object = "T.image")}}{
      draws raster image}
    \item{\code{signature(object = "T.value")}}{
      draws symbols}
    
    \item{\code{signature(object = "Tr.class")}}{
      draws arrows, labels and points}
    \item{\code{signature(object = "Tr.label")}}{
      draws lines, labels and points}
    \item{\code{signature(object = "Tr.match")}}{
      draws arrows, labels and points}
    \item{\code{signature(object = "Tr.traject")}}{
      draws arrows, labels and points}
}}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\keyword{methods}

