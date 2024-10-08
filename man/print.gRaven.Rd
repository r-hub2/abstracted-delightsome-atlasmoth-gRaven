\name{print.gRaven}
\alias{print.gRaven}
\title{Print a gRaven domain}
\description{Print method for a gRaven domain}
\usage{\method{print}{gRaven}(x, ...)}
\arguments{
  \item{x}{gRaven domain}
  \item{...}{additional arguments to \code{\link{print}}}
}
\details{Prints summary information describing the domain.}
\value{a NULL value is invisibly returned.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo(chest,package="gRaven")
print(chest)
}
