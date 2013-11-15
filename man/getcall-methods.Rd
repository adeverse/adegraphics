\name{getcall-methods}
\docType{methods}

\alias{getcall-methods}
\alias{getcall}

\title{Method for \code{ADEg} and \code{ADEgS} objects}

\description{
  \code{getcall} returns the call used to create the object.
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "ADEg")}}{
      returns the slot \code{Call} of the object \code{ADEg}}
    \item{\code{signature(object = "ADEgS")}}{
      returns the slot \code{Call} of the object \code{ADEgS}}
  }
}

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\keyword{methods}

