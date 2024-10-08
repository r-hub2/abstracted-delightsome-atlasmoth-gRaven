\name{get.belief}
\alias{get.belief}
\alias{get.marginal}
\title{Get beliefs in a gRaven domain}
\description{Get beliefs (marginal joint probabilities) in a gRaven domain}
\usage{
get.belief(domain, nodes)
get.marginal(domain, nodes, class = c("data.frame", "table", "ftable", "numeric"))
}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{nodes}{character vector of names of nodes}
  \item{class}{desired class of output}
}
\details{Emulates functions of the same name in the \code{RHugin} package by calls to \code{gRain} functions. Unlike with \code{RHugin}, \code{gRaven} conditions on all entered evidence in reporting probabilities, not only propagated evidence.}
\section{Differences from RHugin}{\code{get.belief} handles more than one node at a time. }
\value{For \code{get.marginal}, a list with one component "table" which is a data frame, table, flat table or numeric vector of marginal joint probabilities, as specified by \code{class}. 
For \code{get.belief}, a vector in the case of a single node (if there is no evidence on that node. a vector of ones), otherwise as \code{get.marginal}. }
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo("chest",package="gRaven",echo=FALSE)
compile(chest)
chest
set.finding(chest,"asia","yes")
set.finding(chest,"dysp","no")
propagate(chest)
get.belief(chest,"asia")
get.belief(chest,"tub")
get.marginal(chest,c('asia','xray','tub'))
}
