\name{randomise}
\alias{randomise}
\title{Randomisation of data}
\description{This function randomise the time order for each gene separately.}

\usage{randomise(eset)}
\arguments{\item{eset}{object of the class \emph{ExpressionSet}.}
 }
 \value{The function produces an object of the ExpressionSet class
   with randomised expression data.}

\author{Matthias E. Futschik
  (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\examples{
data(yeast) # data set includes 17 measurements
yeastR <- randomise(yeast) 
}

\keyword{utilities}
