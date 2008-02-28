\name{filter.NA}
\alias{filter.NA}
\title{Filtering of genes based on number of non-available expression values.}
\description{This function can be used to exclude genes with a large
  number of expression values not available.}

\usage{filter.NA(eset,thres=0.25)}
\arguments{\item{eset}{object of the class \dQuote{ExpressionSet}.}
           \item{thres}{threshold for excluding genes. If the percentage
             of missing values (indicated by NA in the expression matrix)
	     is larger than \code{thres}, the corresponding gene
           will be excluded.}
}
  
\value{The function produces an object of the ExpressionSet class. It is the
  same as the input eset object, except for the genes excluded.}

\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\examples{
if (interactive()){
data(yeast) # data set includes 17 measurements
yeastF <- filter.NA(yeast) # genes are excluded if more than 4 measurements are missing
}
}
\keyword{utilities}
