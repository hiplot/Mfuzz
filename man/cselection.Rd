\name{cselection}
\alias{cselection}
\title{Repeated soft clustering for detection of empty clusters}
\description{This function  performs repeated soft clustering
for a range of cluster numbers c and reports the number of empty clusters
detected.}

\usage{cselection(eset,m,crange=seq(4,32,4),repeats=5,visu=TRUE,\dots)}
\arguments{\item{eset}{object of class \emph{ExpressionSet}.}
\item{m}{value of fuzzy c-means parameter \code{m}.}
\item{crange}{range of number of clusters \code{c}.}
\item{repeats}{number of repeated clusterings.}
\item{visu}{If \code{visu=TRUE} plot of number of empty clusters
is produced.}
\item{\dots}{additional arguments for underlying \code{mfuzz}.}
}

\details{A soft cluster is considered as empty, if none of the genes
has a corresponding membership value larger than 0.5}

\value{A matrix with  the number of empty clusters detected is
generated.}

\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}

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

#### parameter selection
# Empty clusters should not appear
cl <- mfuzz(yeastF,c=20,m=1.25)
mfuzz.plot(yeastF,cl=cl,mfrow=c(4,5))

# Note: The following calculation might take some time

 tmp  <- cselection(yeastF,m=1.25,crange=seq(5,40,5),repeats=5,visu=TRUE)
 # derivation of number of non-empty clusters (crosses) from diagnonal
 # line  indicate appearance of empty clusters 

# Empty clusters might appear 
cl <- mfuzz(yeastF,c=40,m=1.25)
mfuzz.plot(yeastF,cl=cl,mfrow=c(4,5)) 
 }
}
\keyword{cluster}
