\name{compile.gRaven}
\alias{compile.gRaven}
\alias{check.compiled}
\title{Compile a gRaven domain}
\description{Compile a gRaven domain}
\usage{
\method{compile}{gRaven}(object, ...)
check.compiled(object)
}
\arguments{
  \item{object}{name of gRaven domain}
  \item{...}{additional arguments to \code{\link{compile}}}
}
\details{
\code{check.compiled} compiles the domain if necessary, having first ensured that necessary information such as \code{cptables} is in place. If the domain is already compiled, \code{compile} triggers a warning, but proceeds to re-initialise the \code{grain} object \code{domain$net}, in particular deleting any evidence set.
}
\value{a NULL value is invisibly returned.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
chest<-hugin.domain()
add.node(chest,"asia",states=c("yes","no"))
add.node(chest,"tub",states=c("yes","no"))
add.edge(chest,"tub","asia")
chest
compile(chest)
chest
set.finding(chest,"asia","yes")
set.finding(chest,"dysp","no")
propagate(chest)
chest$nodes
chest$states
chest$parents
}
