\name{add.edge}
\alias{add.edge}
\alias{delete.edge}
\title{Add or Delete Edge(s) in a gRaven domain}
\description{Add or delete a directed edge from one or more parents.}
\usage{
add.edge(domain, child, parent)
delete.edge(domain, child, parent)
}
\arguments{
  \item{domain}{name of a gRaven domain}
  \item{child}{a character string containing the name of the child node.}
  \item{parent}{a character string specifying by name the parent nodes}
}
\details{Emulates function of the same name in the \code{RHugin} package by calls to \code{gRain} functions}
\section{Differences from RHugin}{
Deleting an edge nullifies the CPT for node n, if any.
}
\value{a NULL value is invisibly returned.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo(chest.setup,package="gRaven")
chest
delete.edge(chest,"tub","asia")
add.edge(chest,"bronc","asia")
chest
}
