\name{standardise2}
\alias{standardise2}
\title{Standardization in regards to  selected time-point }
\description{Standardisation of the expression values of every gene
  is performed, so that the expression values at a chosen time point
  are zero and the
  standard deviations are one.}

\usage{standardise2(eset,timepoint=1)}
\arguments{\item{eset}{object of the class \emph{exprSet}}
\item{timepoint}{integer:  which time point should have expression
values of zero}
}
  
\value{The function produces an object of the \emph{exprSet} class with
standardised expression values.}

\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\examples{
if (interactive()){
data(yeast)
# Data pre-processing
yeastF <- filter.NA(yeast)
yeastF <- fill.NA(yeastF)
yeastF <- standardise2(yeastF,timepoint=1)

# Soft clustering and visualisation
cl <- mfuzz(yeastF,c=20,m=1.25)
mfuzz.plot(yeastF,cl=cl,mfrow=c(4,5))
}

}
}

\keyword{utilities}
