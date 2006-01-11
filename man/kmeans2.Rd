\name{kmeans2}
\alias{kmeans2}
\title{K-means clustering for gene expression data}
\description{This function is a wrapper function for
  \code{\link[e1071]{kmeans}} of the \code{e1071} package.  It performs
  hard clustering of genes based on their exprssion values using
  the k-means algorithm.}

\usage{kmeans2(eset,k,iter.max=100)}

\arguments{\item{eset}{object of the class \emph{exprSet}}
  \item{k}{number of clusters}
  \item{iter.max}{maximal number of iterations}
}


\value{An list of clustering components (see \code{\link[e1071]{kmeans}}) .}

\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\seealso{ \code{\link[e1071]{kmeans}}}

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
\keyword{cluster}
