\name{fill.NA}
\alias{fill.NA}
\title{Replacement of missing values}
\description{Methods for replacement of missing values.
Missing values should be indicated by NA in the expression matrix.}

\usage{fill.NA(eset,mode="mean",k=10)}
\arguments{\item{eset}{object of the class \emph{ExpressionSet}.}
  \item{mode}{method for replacement of missing values:
    \itemize{\item \emph{mean}-
    missing values will be replaced by the mean expression value of the
    gene,\item \emph{median}-
    missing values will be replaced by the median expression value of the
    gene,\item \emph{knn}-
    missing values will be replaced by the averging over the
    corresponding expression values of the k-nearest neighbours,
    \item \emph{knnw}-same replacement method as
    \emph{knn}, but the expression values averaged are weighted by the
    distance to the corresponding neighbour}
  }
	 \item{k}{Number of neighbours, if one of the knn method for
	 replacement is chosen (\code{knn,knnw}).}
}

\note{ The replacement methods \emph{knn} and \emph{knnw} can
  computationally intensive for large gene expression data sets.
  It may be a good idea to run these methods
  as a \sQuote{lunchtime} or \sQuote{overnight} job.}

\value{The function produces an object of the \emph{ExpressionSet} class with missing
  values replaced.}

\author{Matthias E. Futschik
  (\url{http://itb.biologie.hu-berlin.de/~futschik})  and Lokesh Kumar}

\examples{
if (interactive()){
data(yeast) # data set includes 17 measurements
yeastF <- filter.NA(yeast) 
yeastF <- fill.NA(yeastF)
}
}

\keyword{utilities}
