\name{set.table}
\alias{set.table}
\alias{get.table}
\title{Set or get CPT in a gRaven domain}
\description{Set or get CPT in a gRaven domain}
\usage{
set.table(domain, n, tab = 1, type = c("cpt", "experience", 
    "fading"))
get.table(domain, n, type = c("cpt", "experience", 
    "fading"), class = c("data.frame", "table", 
    "ftable", "numeric"))
}
\arguments{
  \item{domain}{name of gRaven domain}
  \item{n}{name of node}
  \item{tab}{values of conditional probabilties}  
  \item{type}{a character string specifying the type of table to set.}
  \item{class}{a character string specifying the class of the returned table}
}
\section{Differences from RHugin}{
Only type="cpt" and class="data.frame" are currently implemented in \code{gRaven}.
}

\value{For \code{set.table}, a NULL value is invisibly returned; for \code{get.table}, an object of required class.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}

\examples{
chest<-hugin.domain()
add.node(chest,"asia",states=c("yes","no"))
add.node(chest,"tub",states=c("yes","no"))
add.edge(chest,"tub","asia")
compile(chest)
chest
get.table(chest,"asia")
set.table(chest,"asia",c(0.01,0.99))
get.table(chest,"asia")
}
