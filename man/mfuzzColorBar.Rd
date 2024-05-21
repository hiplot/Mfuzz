\name{mfuzzColorBar}
\alias{mfuzzColorBar}
\title{Plots a  colour bar}
\description{This function produces a  (separate) colour bar  for graphs produced by mfuzz.plot}

\usage{mfuzzColorBar(col, horizontal=FALSE,...)}
\arguments{\item{col}{vector of colours used. If missing, the same vector
as the default vector for mfuzz.plot is used. If col="fancy", an
alternative color palette is used (see \code{mfuzz.plot2}.}
\item{horizontal}{If TRUE, a horizontal colour bar is generated,
  otherwise a vertical one will be produced.}
\item{...}{additional parameter passed to \code{maColorBar} (see also example
  in mfuzz.plot2)}
}

\author{Matthias E. Futschik
(\url{http://www.sysbiolab.eu/matthias.html})}

\references{M.E. Futschik and B. Charlisle, Noise robust clustering of gene expression time-course data, Journal of Bioinformatics and Computational Biology, 3 (4), 965-988, 2005

L. Kumar and M. Futschik, Mfuzz: a software package for soft clustering of microarray data, Bioinformation, 2(1) 5-7,2007}


\seealso{ \code{\link[marray]{maColorBar}}}

\examples{
if (interactive()){
 X11(w=1.5,h=5);
 par(mar=c(1,1,1,5))
 mfuzzColorBar()
 mfuzzColorBar(col="fancy",main="Membership value")
 mfuzzColorBar(rev(heat.colors(100)) # example of using heat colors with red indicating high membership values 
}
}
\keyword{utilities}