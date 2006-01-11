\name{overlap}
\alias{overlap} 
\title{Calculation of the overlap of soft clusters}
\description{This function calculates  the overlap of clusters
produced by \code{mfuzz}.}
\usage{overlap(cl)}
\arguments{\item{cl}{object of class \emph{flclust}}	 
}
  
\value{The function generates a matrix of the  normalised overlap
  of soft clusters. The overlap indicates the extent of \dQuote{shared}
  genes between clusters. For a mathematical definiton of the overlap, see the vignette
  of the package or the reference below.}

\author{Matthias E. Futschik  (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\references{M.E. Futschik and B. Charlisle, Noise robust clustering of
gene expression time-course data, Journal of Bioinformatics and
Computational Biology, 3 (4), 965-988, 2005}


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

# Calculation of cluster overlap and visualisation 
O <- overlap(cl)
X11()
Ptmp <- overlap.plot(cl,over=O,thres=0.05)
}
}
\keyword{cluster}
