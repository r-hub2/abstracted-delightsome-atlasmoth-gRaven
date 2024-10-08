\name{initialize.domain}
\alias{initialize.domain}
\title{Re-initialise a gRaven domain}
\description{Re-initialise a gRaven domain}
\usage{initialize.domain(domain)}
\arguments{
  \item{domain}{name of gRaven domain}
}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\details{Emulates function of the same name in the \code{RHugin} package.
Restores the domain to the state it was in after the call to \code{compile}, in particular erasing all entered evidence.}

\value{a NULL value is invisibly returned.}

\examples{
demo("chest",package="gRaven",echo=FALSE)
compile(chest)
chest
set.finding(chest,"asia","yes")
set.finding(chest,"dysp","no")
propagate(chest)
sapply(get.nodes(chest),function(x) get.belief(chest,x)) 
get.normalization.constant(chest)

initialize.domain(chest)
sapply(get.nodes(chest),function(x) get.belief(chest,x)) 
get.normalization.constant(chest)
}
