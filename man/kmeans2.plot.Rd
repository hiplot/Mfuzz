\name{kmeans2.plot}
\alias{kmeans2.plot} 
\title{Plotting results for k-means clustering}
\description{This function visualises the clusters
produced by \code{kmeans2}.}

\usage{kmeans2.plot(eset,kl,mfrow=c(1,1))}
\arguments{\item{eset}{object of the class\dQuote{ExpressionSet}.}
          \item{kl}{list produced by \code{kmeans2}.}
	  \item{mfrow}{determines splitting of graphic window.}
}
  
\value{The function displays the temporal profiles of clusters detected by k-means.}

\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\examples{
if (interactive()){
data(yeast)
# Data pre-processing
yeastF <- filter.NA(yeast)
yeastF <- fill.NA(yeastF)
yeastF <- standardise(yeastF)

# K-means clustering and visualisation
kl <- kmeans2(yeastF,k=20)
kmeans2.plot(yeastF,kl=kl,mfrow=c(2,2))
}
}

\keyword{hplot}
