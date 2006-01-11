\name{filter.std}
\alias{filter.std}
\title{Filtering of genes based on their standard deviation.}
\description{This function can be used to exclude genes with
  low standard deviation.}

\usage{filter.std(eset,min.std,visu=TRUE)}
\arguments{\item{eset}{object of the class \emph{exprSet}}
           \item{min.std}{threshold for minimum standard deviation. If
	     the standard deviation of a gene's expresssion is smaller than
	     \code{min.std}
	     the corresponding gene will be excluded.}
	   \item{visu}{If \code{visu} is set to \code{TRUE},
	     the ordered standard deviations of genes' expression values
	     will be plotted.}
}


\note{As soft clustering is noise robust, pre-filtering can usually be
  avoided.
  However, if the number of genes with small expression changes is
  large, such pre-filtering may be necessary to reduce noise.  }

\value{The function produces an object of the \emph{exprSet} class. It is the
  same as the input \code{eset} object, except for the genes excluded.}

\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\examples{
data(yeast) # data set includes 17 measurements
yeastF <- filter.NA(yeast) # filtering of genes based on missing values 
yeastF <- filter.std(yeastF,min.std=0.3) # filtering of genes based on standard deviation 
}

\keyword{utilities}

