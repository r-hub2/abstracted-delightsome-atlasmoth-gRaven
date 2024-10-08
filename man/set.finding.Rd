\name{set.finding}
\alias{set.finding}
\alias{retract}
\alias{get.finding}
\title{Set, retract and inspect findings}
\description{Set, retract and inspect findings}
\usage{
set.finding(domain, node, finding)
retract(domain, nodes=domain$nodes)
get.finding(domain, nodes=domain$nodes, type = c("entered", "propagated"), 
	namestates=FALSE)
}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{node}{name of node}
  \item{nodes}{character vector naming one or mode nodes}
  \item{finding}{either a state of node \code{node} or a probability distribution over the states, or other non-negative vector of that length.}
  \item{type}{character vector of one or more options to select between evidence entered before or after most recent propagation}
  \item{namestates}{logical, should output have the states labelled}
}
\details{
For \code{type}, partial matching is used.
}
\section{Differences from RHugin}{
Findings (or evidence) are handled differently in \code{gRaven} (and \code{gRain}) than in \code{RHugin}, and that is reflected in differences in results from these functions, especially \code{get.finding}. For \code{set.finding} in \code{gRaven}, the \code{case} argument is not supported. For \code{get.finding} in both packages, previously-set evidence is displayed as a non-negative vector indexed by the states, typically a probability distribution; two enhancements in \code{gRaven} are that findings on more than one node can be displayed, and that the format of the output can be controlled by \code{namestates}. In \code{gRaven} when new evidence is set by \code{set.finding}, it replaces any existing evidence on the same node, and all existing evidence on all nodes is "unpropagated". Evidence is held in a data structure \code{cache} until propagation, when it is moved to \code{evid}; these names are used in labelling the output from \code{get.finding}. Finally if no evidence has been entered on a node, \code{RHugin} reports a vector of all ones, while \code{gRaven} returns such a vector invisibly.
}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\value{For \code{set.finding} and \code{retract}, a NULL value is invisibly returned. For \code{get.finding}, the likelihood vector is invisibly returned for each specified node, in a format governed by \code{namestates}. This is a named vector in the case of a single node, otherwise a list of such vectors. For a single node, in the absence of any evidence, a named vectors of 1's is returned.}
\examples{
demo(chest,package="gRaven",echo=FALSE)
chest
set.finding(chest,"asia","yes")
set.finding(chest,"dysp","no")
propagate(chest)

get.belief(chest,"asia")
get.belief(chest,"tub")
get.finding(chest)

retract(chest,"dysp")
get.belief(chest,"tub")
get.finding(chest)
}
