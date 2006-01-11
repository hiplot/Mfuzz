\name{overlap.plot}
\alias{overlap.plot} 
\title{Visualisation of  cluster overlap and global clustering structure }
\description{This function visualises the cluster overlap 
produced by \code{overlap}.}
\usage{overlap.plot(cl,overlap,thres=0.1,scale=TRUE,magni=30,P=NULL)}
\arguments{\item{cl}{object of class \dQuote{flclust}}
	  \item{overlap}{matrix of cluster overlap produced by \code{overlap} }
	  \item{thres}{threshold for visualisation. Cluster
            overlaps below the threshold will not be visualised.}
	  \item{scale}{Scale parameter for principal component
analysis by \code{\link{prcomp}}}
\item{magni}{Factor for increase the line width for cluster overlap.} 
\item{P}{Projection  matrix produced by principal component analysis.}
}
  
\value{A plot is genererated based on a prinicpal component analysis of
the
cluster centers. The overlap is visualised by lines with variable
width indicating the strength of the overlap. Additonally, the matrix
of principal components is returned. This matrix can be re-used for other
projections to compare the overlap and global cluster structure of
different
clusterings. }

\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\seealso{\code{\link{prcomp}}}
\examples{
if (interactive()){
data(yeast)
# Data pre-processing
yeastF <- filter.NA(yeast)
yeastF <- fill.NA(yeastF)
yeastF <- standardise(yeastF)

# Soft clustering
cl <- mfuzz(yeastF,c=20,m=1.25)
X11();mfuzz.plot(yeastF,cl=cl,mfrow=c(4,5))
O <- overlap(cl)
X11();Ptmp <- overlap.plot(cl,over=O,thres=0.05)

# Alternative clustering 
cl <- mfuzz(yeastF,c=10,m=1.25)
X11();mfuzz.plot(yeastF,cl=cl,mfrow=c(3,4))
O <- overlap(cl)

X11();overlap.plot(cl,over=O,P=Ptmp,thres=0.05)
# visualisation based on  principal compents from previous projection
}
}
\keyword{hplot}
