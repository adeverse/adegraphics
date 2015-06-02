\name{sortparamADEg}

\alias{sortparamADEg}
\alias{sortparamADEgS}

\title{
Sort a sequence of graphical parameters
}

\description{
Sort a sequence of graphical parameters in several lists.
}

\usage{
sortparamADEg(...)
sortparamADEgS(..., graphsnames, nbsubgraphs = rep(1, length(graphsnames)))
}

\arguments{
  \item{...}{a sequence of graphical parameters}
  
  \item{graphsnames}{a sequence containing the name of each simple graph of the ADEgS}
  
  \item{nbsubgraphs}{a sequence containing the number of sub-graphs in each graph named in \code{graphsnames}}
      
}

\value{
  \code{sortparamADEg} return a list of four lists named \code{adepar}, \code{trellis}, \code{g.args} and \code{rest}.
  \code{sortparamADEgS} return a list of as many lists as the length of \code{graphsnames}, i.e., as the number of 
  sub-graphs of the ADEgS. The names of the lists are \code{graphsnames} and each sub-list is the result of the 
  \code{sortparamADEg} function aplly on each sub-graph.
  }

\author{Alice Julien-Laferriere, Aurelie Siberchicot \email{aurelie.siberchicot@univ-lyon1.fr} and Stephane Dray
}

\examples{

l1 <- sortparamADEg(xlab = "x-axis label", ylab = "y-axis label", plabels.cex = 1.5, 
  porigin.include = FALSE)
length(l1)
names(l1)
  
l2 <- sortparamADEgS(xlab = "x-axis label", eig.main = "Eigenvalues", row.ppoints.col = "red",
  porigin.include = FALSE, graphsnames = c("row", "col", "eig"))
names(l2)
names(l2$row)

l3 <- sortparamADEgS(xlab = "x-axis label", eig.main = "Eigenvalues", row.ppoints.col = "pink",
  porigin.include = FALSE, graphsnames = c("row", "col", "eig"), nbsubgraphs = c(1, 2, 1))
names(l3)
length(l3$row)
length(l3$col)
}

\keyword{aplot}
