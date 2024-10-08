\name{simulate.gRaven}
\alias{simulate.gRaven}
\title{ method for a gRaven domain}
\description{Simulate method for a gRaven domain}
\usage{\method{simulate}{gRaven}(object, nsim = 1, seed = NULL, ...)}
\arguments{
  \item{object}{character string, name of gRaven domain}
  \item{nsim}{Number of cases to simulate}
  \item{seed}{An optional integer controlling the random number generatation}
  \item{...}{additional arguments to \code{\link{simulate}}}
}
\value{a data frame.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo("chest",package="gRaven",echo=FALSE)
compile(chest)
chest
set.finding(chest,"asia","yes")
set.finding(chest,"dysp","no")
newdata<-simulate(chest,100)
head(newdata)
}
