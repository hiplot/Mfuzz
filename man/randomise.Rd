\name{randomise}
\alias{randomise}
\title{Randomisation of data}
\description{This function randomise the time order for each gene separately.}

\usage{randomise(eset)}
\arguments{\item{eset}{object of the class \emph{exprSet}}
 }
 \value{The function produces an object of the exprSet class
   with randomised expression data.}

\author{Matthias E. Futschik
  (\url{http://itb.biologie.hu-berlin.de/~futschik}) }

\examples{
data(yeast) # data set includes 17 measurements
yeastR <- randomise(yeast) 
}

\keyword{utilities}
