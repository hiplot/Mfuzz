\name{standardise}
\alias{standardise}
\title{Standardization of microarray data for clustering.}
\description{Standardisation of the expression values of every gene
  is performed, so that the average expression value for each gene
  is zero and the
  standard deviation is one.}

\usage{standardise(eset)}
\arguments{\item{eset}{object of the classe \emph{exprSet}}
           }
  
\value{The function produces an object of the exprSet class with
standardised expression values.}

\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\examples{
if (interactive()){
data(yeast)
# Data pre-processing
yeastF <- filter.NA(yeast)
yeastF <- fill.NA(yeastF)
yeastF <- standardise(yeastF)

# Soft clustering and visualisation
cl <- mfuzz(yeastF,c=20,m=1.25)
mfuzz.plot(yeastF,cl=cl,mfrow=c(4,5))
}
}
\keyword{utilities}
