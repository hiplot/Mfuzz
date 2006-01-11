\name{partcoef}
\alias{partcoef}
\title{Calculation of the partition coefficient matrix for soft clustering}
\description{This function calculates partition coefficient for
  clusters within a range of cluster parameters. It can be used to
determine the parameters which lead to uniform clustering.}
\usage{partcoef(eset,crange=seq(4,32,4),mrange=seq(1.05,2,0.1),...)}
\arguments{\item{eset}{object of class \dQuote{exprSet}}
\item{crange}{range of number of clusters \code{c}}
\item{mrange}{range of clustering paramter \code{m}}
\item{...}{additional arguments for underlying \code{mfuzz}}	 
}


\details{Introduced by Bezdek (1981), the partition coefficient F is
defined as the sum of squares of values of the partition matrix devided
by the number of values. It is maximal if the partition is hard and
reaches a minimum for U=1/c  when every gene is equally assigned to every
cluster.

It is well-known that the partition coefficient tends to
decrease monotonically with increasing n. To reduce this tendency we
defined a normalized partition coefficient where the particion for
uniform
partitions are subracted from the actual partition coefficients
(Futschik and Kasabov,2002).} 

  
\value{The function generates the matrix of partition coefficients  for
a range of \code{c} and \code{m} values. It also produces a matrix of  normalised
partition coefficients  as well as a matrix with partition coefficient for  uniform partitions.}




\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\references{
\enumerate{
\item J.C.Bezdek, Pattern recognition with fuzzy objective
function algorithms, Plenum, 1981
\item M.E. Futschik and N.K. Kasabov. Fuzzy clustering of gene
expression data, Proceedings of World Congress of Computational
Intelligence WCCI 2002, Hawaii, IEEE Press, 2002}
}

\examples{
if (interactive()){
data(yeast)
# Data pre-processing
yeastF <- filter.NA(yeast)
yeastF <- fill.NA(yeastF)
yeastF <- standardise(yeastF)

#### parameter selection
yeastFR <- randomise(yeastF)
cl <- mfuzz(yeastFR,c=20,m=1.1)
mfuzz.plot(yeastFR,cl=cl,mfrow=c(4,5)) # shows cluster structures (non-uniform partition)


 tmp  <- partcoef(yeastFR) # This might take some time.
 F <- tmp[[1]];F.n <- tmp[[2]];F.min <- tmp[[3]]

 # Which clustering parameters result in a uniform partition?  
 F > 1.01 * F.min


cl <- mfuzz(yeastFR,c=20,m=1.25) # produces uniform partion 

mfuzz.plot(yeastFR,cl=cl,mfrow=c(4,5))
# uniform coloring of temporal profiles indicates uniform partition
}
}
\keyword{cluster}
