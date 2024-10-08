\name{hugin.domain}
\alias{hugin.domain}
\alias{clone.domain}
\title{Create or Clone a gRaven Domain}
\description{Create or Clone a gRaven domain object.}
\usage{
hugin.domain()
clone.domain(domain)
}
\arguments{
  \item{domain}{name of gRaven domain}
}
\details{Emulates functions of the same name in the \code{RHugin} package. A gRaven domain is an environment, with additional class attribute 'gRaven'. The environment holds structures such as \code{nodes}, \code{states}, \code{parents} and \code{cptables}, populated incrementally by functions in the package, preparing the information needed by the \code{gRain} package to create and manipulate the structure \code{net} in the domain, which is a \code{grain} object.}
\value{character string naming a gRaven domain}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
chest<-hugin.domain()

yn <- c("yes","no")
add.node(chest,"asia",states=yn)
add.node(chest,"smoke",states=yn)
add.node(chest,"tub",states=yn)
add.node(chest,"lung",states=yn)
add.node(chest,"either",states=yn)

add.edge(chest,"tub","asia")
add.edge(chest,"lung","smoke")
add.edge(chest,"either",c("lung","tub"))

set.table(chest,"asia",c(0.01,0.99))
set.table(chest,"tub",c(0.05,0.95,0.01,0.99))
set.table(chest,"smoke",c(0.5,0.5))
set.table(chest,"lung",c(0.1,0.9,0.01,0.99))
set.table(chest,"either",c(1,0,1,0,1,0,0,1))

chest

chest2<-clone.domain(chest)

add.node(chest2,"bronc",states=yn)
add.node(chest2,"dysp",states=yn)
add.node(chest2,"xray",states=yn)

add.edge(chest2,"bronc","smoke")
add.edge(chest2,"dysp",c("bronc","either"))
add.edge(chest2,"xray","either")

set.table(chest2,"bronc",c(0.6,0.4,0.3,0.7))
set.table(chest2,"dysp",c(0.9,0.1,0.7,0.3,0.8,0.2,0.1,0.9))
set.table(chest2,"xray",c(0.98,0.02,0.05,0.95))

chest2
}
