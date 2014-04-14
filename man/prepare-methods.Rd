\name{prepare-methods}
\docType{methods}

\alias{prepare-methods}
\alias{prepare}

\title{Methods \code{prepare} for \code{ADEg} objects}

\description{
  The method \code{prepare} performs the first calculus needed for the display.
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "ADEg.C1")}}{
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}
    \item{\code{signature(object = "C1.barchart")}}{
      calls the parent method (\code{prepare} for \code{ADEg.C1}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "C1.curve")}}{
      calls the parent method (\code{prepare} for \code{ADEg.C1}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "C1.density")}}{
      calls the parent method (\code{prepare} for \code{ADEg.C1}), modifies some graphical parameters used by default and 
      calculates the density curves according to the numeric score and the values' categories}
    \item{\code{signature(object = "C1.dotplot")}}{
      calls the parent method (\code{prepare} for \code{ADEg.C1}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "C1.gauss")}}{
      calls the parent method (\code{prepare} for \code{ADEg.C1}), modifies some graphical parameters used by default and 
      calculates the Gauss curves according to the numeric score and the values' categories (using weighted mean and standard deviation)}
    \item{\code{signature(object = "C1.hist")}}{
      calls the parent method (\code{prepare} for \code{ADEg.C1}), modifies some graphical parameters used by default 
      and calculates the boundaries and the height of cells}
    \item{\code{signature(object = "C1.interval")}}{
      calls the parent method (\code{prepare} for \code{ADEg.C1}) and modifies some graphical parameters used by default}  
    
    \item{\code{signature(object = "ADEg.S1")}}{
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}
    \item{\code{signature(object = "S1.boxplot")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S1}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "S1.class")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S1}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "S1.distri")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S1}), modifies some graphical parameters used by default and calculates weighted mean and standard deviation}
    \item{\code{signature(object = "S1.label")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S1}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "S1.match")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S1}) and modifies some graphical parameters used by default}
    
    \item{\code{signature(object = "ADEg.S2")}}{
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}
    \item{\code{signature(object = "S2.arrow")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and calculates limits}
    \item{\code{signature(object = "S2.class")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and calculates ellipses, convex hulls and centroids}
    \item{\code{signature(object = "S2.corcircle")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and prepares the drawn grid}
    \item{\code{signature(object = "S2.density")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and calculates densities}
    \item{\code{signature(object = "S2.distri")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and calculates ellipses and centroids}
    \item{\code{signature(object = "S2.image")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and calculates grid expansion and limits}
    \item{\code{signature(object = "S2.label")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "S2.logo")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "S2.match")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "S2.traject")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "S2.value")}}{
      calls the parent method (\code{prepare} for \code{ADEg.S2}), modifies some graphical parameters used by default and calculates limits}
    
    \item{\code{signature(object = "ADEg.T")}}{
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}
    \item{\code{signature(object = "T.image")}}{
      calls the parent method (\code{prepare} for \code{ADEg.T}) and modifies some graphical parameters used by default and calculates limits and grid}
    \item{\code{signature(object = "T.value")}}{
      calls the parent method (\code{prepare} for \code{ADEg.T}) and modifies some graphical parameters used by default and calculates limits and grid}
    
    \item{\code{signature(object = "ADEg.Tr")}}{
      performs the calculations before display the object (e.g. limits, grid and axis calculations)}
    \item{\code{signature(object = "Tr.class")}}{
      calls the parent method (\code{prepare} for \code{ADEg.Tr}), modifies some graphical parameters used by default and calculated ellipses, convex hulls and centroids}
    \item{\code{signature(object = "Tr.label")}}{
      calls the parent method (\code{prepare} for \code{ADEg.Tr}) and modifies some graphical parameters used by default}
    \item{\code{signature(object = "Tr.match")}}{
      calls the parent method (\code{prepare} for \code{ADEg.Tr}), modifies some graphical parameters used by default and defines the mean point and the axis}
    \item{\code{signature(object = "Tr.traject")}}{
      calls the parent method (\code{prepare} for \code{ADEg.Tr}) and modifies some graphical parameters used by default}
}}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\keyword{methods}

