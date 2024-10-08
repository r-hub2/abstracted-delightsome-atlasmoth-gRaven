\name{map.configurations}
\alias{map.configurations}
\title{Get belief in a gRaven domain}
\description{Find the configurations of the specified nodes that occur with probability \code{pmin} or greater. These configurations are known as most probable configurations or maximum a posteriori (MAP) configurations.}
\usage{map.configurations(domain, nodes, pmin)}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{nodes}{character vector of names of nodes}
  \item{pmin}{a single numeric value between 0 and 1 specifying the minimum probability for the most probable configurations}
}
\details{Emulates function of the same name in the \code{RHugin} package by calls to \code{gRain} functions}
\value{Data.frame with one column for each node in \code{nodes}. Each row contains a most probable configuration. The final column of the data.frame (\code{Prob}) gives the probability of the configuration.}
\author{Therese Graversen, \email{theg@itu.dk}}
\examples{
demo("chest",package="gRaven",echo=FALSE)
set.finding(chest,"asia","yes")
set.finding(chest,"dysp","no")
map.configurations(chest,c("lung","bronc"),0.0001)
}
