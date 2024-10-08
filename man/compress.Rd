\name{compress}
\alias{compress}
\title{Dummy network compression routine}
\description{Dummy routine.}
\usage{compress(domain)}
\arguments{
  \item{domain}{name of gRaven domain}
}
\details{compression in the sense used in Hugin is not available in
gRain, so this function has no effect.}
\value{1}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo("chest",package="gRaven",echo=FALSE)
compile(chest)
compress(chest)
}
