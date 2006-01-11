\name{top.count}
\alias{top.count} 
\title{Determines the number for which each gene has highest membership value in all cluster}
\description{This function calculates the number,for which each gene appears to have the top 
membership score in the partition matrix of clusters produced by \code{mfuzz}.}
\usage{top.count(cl)}
\arguments{\item{cl}{object of class \dQuote{flclust}}	 
}
  
\value{The function generates a vector containing a count for each gene, which is just the 
number of times that particular gene has acquired the top membership score.}

\author{Lokesh Kumar and Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\examples{
if (interactive()){
data(yeast)
# Data pre-processing
yeastF <- filter.NA(yeast)
yeastF <- fill.NA(yeastF)
yeastF <- standardise(yeastF)

# Soft clustering and visualisation
cl <- mfuzz(yeastF,c=20,m=1.25)
top.count(cl)
}
}
\keyword{cluster}
 
