\name{Dmin}
\alias{Dmin}
\title{Calculation of minimum centroid distance for a range of cluster
numbers for estimation of optimised number of clusters}
\description{This function  performs repeated soft clustering
for a range of cluster numbers c and reports the minimum centroid distance.}

\usage{Dmin(eset,m,crange=seq(4,40,4),repeats=3,visu=TRUE)}
\arguments{\item{eset}{object of class \emph{ExpressionSet}.}
\item{m}{value of fuzzy c-means parameter \code{m}.}
\item{crange}{range of number of clusters \code{c}.}
\item{repeats}{number of repeated clusterings.}
\item{visu}{If \code{visu=TRUE} plot of average minimum centroid
distance is produced}
}

\details{The minimum centroid distance is defined as the minimum
  distance between two cluster centers produced by the c-means
  clusterings.}

\value{The average minimum centroid distance for the given range
of cluster number is returned.}

\note{The minimum centroid distance can be used as cluster validity
index.  For an optimal cluster number, we may see a \sQuote{drop} of minimum centroid distance
wh plotted versus a range of cluster number and a slower
decrease of the minimum centroid distance for higher cluster number.
More information and some examples can be found in the study of
Schwaemmle and Jensen (2010). 
However, it should be used with care, as the determination remains
difficult especially for short time series and overlapping
clusters. Alternatively, the function \code{cselection} can be used or
functional enrichment analysis (e.g. using Gene Ontology) can help to
adjust the cluster number.}

\author{Matthias E. Futschik (\url{http://www.sysbiolab.eu/matthias.html})}

\references{M.E. Futschik and B. Charlisle, Noise robust clustering of
gene expression time-course data, Journal of Bioinformatics and
Computational Biology, 3 (4), 965-988, 2005

L. Kumar and M. Futschik, Mfuzz: a software package for soft clustering of microarray data, Bioinformation, 2(1) 5-7,2007
   
Schwaemmle and Jensen, Bioinformatics,Vol. 26 (22), 2841-2848, 2010 
}


\examples{
if (interactive()){
data(yeast)
# Data pre-processing
yeastF <- filter.NA(yeast)
yeastF <- fill.NA(yeastF)
yeastF <- standardise(yeastF)

#### parameter selection
# For fuzzifier m, we could use mestimate
m1 <- mestimate(yeastF)
m1 # 1.15

# or the function partcoef (see example there)

# For selection of c, either cselection (see example there)
# or

 tmp  <- Dmin(yeastF,m=m1,crange=seq(4,40,4),repeats=3,visu=TRUE)# Note: This calculation might take some time

 # It seems that the decrease for c ~ 20 - 25 24 and thus 20 might be
 # a suitable number of clusters 
 
}
\keyword{cluster}
