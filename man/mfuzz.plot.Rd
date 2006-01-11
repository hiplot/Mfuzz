\name{mfuzz.plot}
\alias{mfuzz.plot} 
\title{Plotting results for soft clustering}
\description{This function visualises the clusters
produced by \code{mfuzz}.}

\usage{mfuzz.plot(eset,cl,mfrow=c(1,1),colo,min.mem=0,time.labels,new.window=TRUE)}
\arguments{\item{eset}{object of the class\emph{exprSet}}
          \item{cl}{object of class \emph{flclust}}
	  \item{mfrow}{determines splitting of graphic window}
	  \item{colo}{color palette to be used for plotting. If the
	    color argument remains empty, the default palette is used.}
	  \item{min.mem}{Genes with membership values below
	    \code{min.mem} will not be displayed.}
	  \item{time.labels}{labels can be given for the time axis}
	  \item{new.window} {should a new window be opened for graphics} 
}
  
\value{The function generates plots where the membership of genes
is color-encoded.}

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
mfuzz.plot(yeastF,cl=cl,mfrow=c(2,2))

# display of cluster cores with alpha = 0.5
mfuzz.plot(yeastF,cl=cl,mfrow=c(2,2),min.mem=0.5)

# display of cluster cores with alpha = 0.7
mfuzz.plot(yeastF,cl=cl,mfrow=c(2,2),min.mem=0.7)
}
}
\keyword{hplot}
