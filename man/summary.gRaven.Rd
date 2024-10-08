\name{summary.gRaven}
\alias{summary.gRaven}
\title{Summary method for a gRaven domain}
\description{Summary method for a gRaven domain}
\usage{\method{summary}{gRaven}(object, domain = TRUE, nodes = FALSE, jt = FALSE, print.cliques = FALSE, 
    ...)}
\arguments{
  \item{object}{character string, name of gRaven domain}
  \item{domain}{Logical, should domain be summarised}
  \item{jt}{Logical, should junction tree be summarised}
  \item{nodes}{Logical, should nodes be summarised}
  \item{print.cliques}{Logical, should cliques be printed}
  \item{...}{additional arguments to \code{\link{summary}}}
}
\section{Differences from RHugin}{
In \code{gRaven}, not all of the options are yet implemented, and generally the information delivered is less detailed than in the \code{RHugin} version.
}
\value{a data frame.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo("chest",package="gRaven",echo=FALSE)
chest
summary(chest,jt=TRUE)
}
