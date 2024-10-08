\name{propagate.gRaven}
\alias{propagate.gRaven}
\title{Propagate method for a gRaven domain}
\description{Propagate method for a gRaven domain}
\usage{\method{propagate}{gRaven}(object, ...)}
\arguments{
  \item{object}{character string, name of gRaven domain}
  \item{...}{additional arguments to \code{\link[gRbase]{propagate}}}
}
\details{Propagates all previously entered evidence through the network. This call is required by functions such as \code{get.belief} and \code{map.configurations}, but not before \code{get.normalization.constant}. In the implementation, findings will have been accumulated in the variable net$cache in the domain, by calls to \code{set.finding}, and are moved to net$evid by the \code{propagate} function.}
\value{a NULL value is invisibly returned.}
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
}
