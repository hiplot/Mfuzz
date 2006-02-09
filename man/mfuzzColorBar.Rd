\name{mfuzzColorBar}
\alias{mfuzzColorBar}
\title{Plots a  colour bar}
\description{This function produces a  (separate) colour bar  for graphs produced by mfuzz.plot}

\usage{mfuzzColorBar(col, horizontal=FALSE)}
\arguments{\item{col}{vector of colours used. If missing, the same vector
as the default vector for mfuzz.plot is used}
\item{horizontal}{If TRUE, a horizontal colour bar is generated,
otherwise a vertical one will be produced.}
}

\author{Matthias E. Futschik
(\url{http://itb.biologie.hu-berlin.de/~futschik})}

\seealso{ \code{\link[marray]{maColorBar}}}

\examples{
if (interactive()){
 X11(w=1.5,h=5);
 par(mar=c(1,1,1,5))
 mfuzzColorBar()
}
}
\keyword{utilities}