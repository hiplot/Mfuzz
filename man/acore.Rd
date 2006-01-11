\name{acore}
\alias{acore}
\title{Extraction of alpha cores for soft clusters}
\description{This function extracts genes forming the alpha cores of
  soft clusters }

\usage{acore(eset,cl,min.acore=0.5)}
\arguments{\item{eset}{object of the class \emph{exprSet}}
  \item{cl}{An object of class \code{flcust} as produced by \code{mfuzz}.}
\item{min.acore}{minimum membership values of  gene belonging to the
  cluster core}
 }


\value{The function produces an list of alpha cores  including genes and
their membership values for the corresponding cluster.  }

\author{Matthias E. Futschik
  (\url{http://itb.biologie.hu-berlin.de/~futschik})  }

\examples{
if (interactive()){
### Data loaing and pre-processing
 data(yeast) # data set includes 17 measurements
 yeastF <- filter.NA(yeast) 
 yeastF <- fill.NA(yeastF)
 yeastF <- standardise(yeastF)

### Soft clustering and visualisation
cl <- mfuzz(yeastF,c=20,m=1.25)
acore.list <- acore(yeastF,cl=cl,min.acore=0.7)
 }
}
\keyword{utilities}
