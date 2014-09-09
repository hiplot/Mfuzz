\name{membership}
\alias{membership}
\title{Calculating of membership values for new data based on existing clustering}
\description{Function that calculates the membership values of genes based on provided data and existing clustering}

\usage{membership(x,clusters,m)}
\arguments{\item{x}{expression vector or expression matrix}
  \item{clusters}{cluster centroids from existing clustering}
   \item{m}{fuzzification parameter }
}

\note{This function calculates membership values for new data based on
    existing cluster centroids and fuzzification parameter. It can be useful, for instance, when comparing two time series, to assess whether the same
    gene in the different time series changes its cluster association.}

\value{Matrix of membership values for new genes}

\author{Matthias E. Futschik
  (\url{http://www.sysbiolab.eu)}}

\examples{
if (interactive()){
 data(yeast)
 yeastF <- filter.NA(yeast)
 yeastF <- fill.NA(yeastF) # for illustration only; rather use knn method
 yeastF <- standardise(yeastF)
 
 cl <- mfuzz(yeastF,c=20,m=1.25)

 m <- 1.25
 clusters <- cl[[1]]
 x <- matrix(rnorm(2*17),nrow=2) # new expression matrix with two genes 
 mem.tmp <- membership(x,clusters=clusters,m=m) #membership values  
}
}

\keyword{utilities}
