\name{standardise}
\alias{standardise}
\title{Standardization of expression data for clustering.}
\description{Standardisation of the expression values of every
  gene/transcript/protein is carried out, so that the average expression
  value for each gene/transcript/protein is zero and the standard
  deviation of its expression profile is one.}

\usage{standardise(eset)}
\arguments{\item{eset}{object of the classe \emph{ExpressionSet}.}
}


\value{The function produces an object of the ExpressionSet class with
  standardised expression values.}

\note{Mfuzz assumes that the given expression data are preprocessed
(including the normalisation). The function \code{standardise} does not
replace the normalisation step. Note the difference: Normalisation is carried
out to make different samples comparable, while standardisation (in Mfuzz)
is carried out to make transcripts (genes) comparable.}

\author{Matthias E. Futschik  (\url{http://www.sysbiolab.eu})}

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
}
}
\keyword{utilities}
