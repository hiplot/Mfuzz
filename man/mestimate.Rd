\name{mestimate}
\alias{mestimate}
\title{Estimate for optimal fuzzifier m}
\description{This function estimates an optimal setting of fuzzifier m}

\usage{mestimate(eset)}
\arguments{\item{eset}{object of class \dQuote{ExpressionSet}}
}

\details{Schwaemmle and Jensen proposed an method to estimate of
\emph{m},
which was motivated by the evaluation of fuzzy clustering applied to randomized
  datasets. The estimated \emph{m} should give the minimum fuzzifier value which
  prevents clustering of randomized data. } 

\value{Estimate for optimal fuzzifier.}

\author{Matthias E. Futschik (\url{http://itb.biologie.hu-berlin.de/~futschik})}

\references{
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

#### parameter selection
# For fuzzifier m, we could use mestimate
m1 <- mestimate(yeastF)
m1 # 1.15

cl <- mfuzz(yeastF,c=20,m=m1)
mfuzz.plot(yeastF,cl=cl,mfrow=c(4,5))
}
}
\keyword{cluster}
