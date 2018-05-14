\name{setlimits1D}

\alias{setlimits1D}
\alias{setlimits2D}

\title{
Computes limits for 1D and 2D displays.
}

\description{
Computes limits for 1D and 2D displays adding 10\% of margins around the extreme values.
}

\usage{
setlimits1D(mini, maxi, origin, includeOr)
setlimits2D(minX, maxX, minY, maxY, origin = c(0, 0), aspect.ratio = "iso", includeOr)
}

\arguments{
  \item{mini}{the smallest value of a unidimensional dataset}
  \item{maxi}{the largest value of a unidimensional dataset}
  \item{minX}{the smallest value of the first dimension of a bidimensional dataset}
  \item{maxX}{the largest value of the first dimension of a bidimensional dataset}
  \item{minY}{the smallest value of the second dimension of a bidimensional dataset}
  \item{maxY}{the largest value of the second dimension of a bidimensional dataset}
  \item{origin}{a value (in \code{setlimits1D}) or a two-length vector (in \code{setlimits2D}) indicating origin coordinate(s)}
  \item{aspect.ratio}{a character string to control physical aspect ratio of the graphic.
        \code{iso} for isometric scales, \code{fill} for drawing as big as possible or \code{xy} for banking rule}
  \item{includeOr}{a boolean value indicating whether the origin is included in the graphics window}
}

\value{
  \code{setlimits1D} returns a two-length vector containing the limits of the graphics window on one axis. \cr
  \code{setlimits2D} returns a two-length list where the first element, named \code{xlim}, contains a two-length vector 
  containing the limits of the graphics window on the first axis and the second, named \code{ylim}, contains the limits on 
  the second axis.
  }

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\examples{
setlimits1D(mini = -2, maxi = 2, origin = 0, includeOr = TRUE)
setlimits2D(minX = -2, maxX = 2, minY = -3, maxY = 4, origin = c(0, 0), includeOr = TRUE)
}

\keyword{aplot}
