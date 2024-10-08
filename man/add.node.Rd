\name{add.node}
\alias{add.node}
\alias{delete.node}
\title{Add or Delete Node in a gRaven domain}
\description{Add or delete a node in a gRaven domain.}
\usage{
add.node(domain, name, category = c("chance", 
    "decision", "utility", "function"), kind = c("discrete", "continuous", "other"), 
    subtype, states)
delete.node(domain, name)
}
\arguments{
  \item{domain}{name of a gRaven domain}
  \item{name}{a character string containing the name of the node to be added or deleted}
  \item{category}{a character string specifying the category of the node}
  \item{kind}{a character string specifying the kind of the node}
  \item{subtype}{a character string, 'labeled', 'numbered' or 'boolean'}
  \item{states}{a character, numeric or logical vector listing the states for this node}
}
\details{Emulates functions of the same name in the \code{RHugin} package by calls to \code{gRain} functions, \code{delete.node} deletes the specified node and all incident edges; nullifies all corresponding CPTs.}
\section{Differences from RHugin}{
Only discrete chance nodes are currently handled in \code{gRaven}.
}
\value{a NULL value is invisibly returned.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo(chest.setup,package="gRaven")
chest
add.node(chest,'dummy',states=1:3)
delete.node(chest,'either')
chest
}
