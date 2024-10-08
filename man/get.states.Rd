\name{get.states}
\alias{get.states}
\alias{get.children}
\alias{get.edges}
\title{Get states, edges and children in a gRaven domain}
\description{Get states, edges and children in a gRaven domain}
\usage{
get.states(domain, nodes=domain$nodes)
get.children(domain, nodes)
get.edges(domain, nodes=domain$nodes)
}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{nodes}{name of node(s)}
}
\details{Emulates functions of the same name in the \code{RHugin} package by calls to \code{gRain} functions}
\section{Differences from RHugin}{
get.states and get.children allow more than one node; get.edges allows selecting nodes.
}
\value{For \code{get.states}, vector of state values. For \code{get.edges},
a list with one element for each node in \code{domain}. Each element is in turn a list with a single element \code{edges} which is a character vector of names of the node's children.
An empty vector indicates that the node has no children. For \code{get.children},
character vector of children's node names in the case of a single node, otherwise list of children of each node, each element being a character vector of names of the node's children.
}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo("chest",package="gRaven",echo=FALSE)
get.states(chest,"asia")
get.children(chest,"asia")
get.edges(chest)
}
