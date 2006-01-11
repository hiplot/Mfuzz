\name{mfuzz}
\alias{mfuzz}
\title{Function for soft clustering based on fuzzy c-means.}
\description{This function is a wrapper function for
  \code{\link[e1071]{cmeans}} of the \code{e1071} package.  It performs
  soft clustering of genes based on their exprssion values using
  the fuzzy c-means algorithm.}

\usage{mfuzz(eset,centers,m,...)}

\arguments{\item{eset}{object of the class \dQuote{exprSet}}
  \item{centers}{number of clusters}
  \item{m}{fuzzification parameter}
  \item{...}{additional parameters for \code{\link[e1071]{cmeans}}}
}

\details{This function is the core function for soft clustering.
  It groups genes based on the Euclidean distance and the c-means
  objective
  function which is a weighted square error function. Each gene
  is assigned a membership value between 0 and 1 for each cluster.
  Hence, genes can be assigned to different clusters in a gradual
  manner. 
  This contrasts hard clustering where each gene can belongs to a single
  cluster.}

\value{An object of class \code{flcust} (see \code{\link[e1071]{cmeans}}) .}

\author{Matthias E. Futschik
(\url{http://itb.biologie.hu-berlin.de/~futschik})}

\references{M.E. Futschik and B. Charlisle, Noise robust clustering of
gene expression time-course data, Journal of Bioinformatics and
Computational Biology, 3 (4), 965-988, 2005}

\seealso{ \code{\link[e1071]{cmeans}}}

\examples{
if (interactive()){
data(yeast)
# Data pre-processing
yeastF <- filter.NA(yeast)
yeastF <- fill.NA(yeastF)
yeastF <- standardise(yeastF)

# Soft clustering and visualisation
cl <- mfuzz(yeastF,c=20,m=1.25)
mfuzz.plot(yeastF,cl=cl,mfrow=c(2,2))
}
}

\keyword{cluster}
