\name{mfuzz.plot2}
\alias{mfuzz.plot2} 
\title{Plotting results for soft clustering with additional options}
\description{This function visualises the clusters
produced by \code{mfuzz}. it is similar to \code{mfuzz.plot}, but offers
more options for adjusting the plots.}

\usage{mfuzz.plot2(eset,cl,mfrow=c(1,1),colo,min.mem=0,time.labels,x11=TRUE,
                        ax.col="black",bg = "white",col.axis="black",col.lab="black",
                        col.main="black",col.sub="black",col="black",cex.main=2,
                        Xwidth=5,Xheight=5,single=FALSE,...)}


\arguments{\item{eset}{object of the class\emph{exprSet}}
          \item{cl}{object of class \emph{flclust}}
	  \item{mfrow}{determines splitting of graphic window. Use
	    \code{mfrow}=NA if \code{layout} is used (see example).}
	  \item{colo}{color palette to be used for plotting. If the
	    color argument remains empty, the default palette is
	    used. If the \code{colo = "fancy"}, an alternative (fancier)
	  palette will be used.}
	  \item{min.mem}{Genes with membership values below
	    \code{min.mem} will not be displayed.}
	  \item{time.labels}{labels can be given for the time axis}
	  \item{x11} {If TRUE, a new window will be open for plotting.}
	  \item{ax.col}{Color of axis line}
	  \item{bg}{Background color}
	  \item{col.axis}{Color for axis annotation}
	  \item{col.lab}{Color for axis labels}
	  \item{col.main}{Color for main titles}
	  \item{col.sub}{Color for sub-titles}
	  \item{col}{Default plotting color}
	  \item{cex.main}{Magnification to be used for main titles}
	  \item{Xwidth}{Width of window}
	  \item{Xheight}{Height of window}
	  \item{single}{Integer if a specific cluster is to be plotted,
	    otherwise it should be set to FALSE.}
	  \item{...}{Additional, optional plotting arguments passed to plot.default
	    function. }

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
mfuzz.plot2(yeastF,cl=cl,mfrow=c(2,2)) # same output as mfuzz.plot


# More fancy choice of colors
mfuzz.plot2(yeastF,cl=cl,mfrow=c(2,2),colo="fancy",
ax.col="red",bg = "black",col.axis="red",col.lab="white",
col.main="green",col.sub="blue",col="blue",cex.main=2)

### Single cluster  with colorbar (cluster # 3) 
X11(width=12)
mat <- matrix(1:2,ncol=2,nrow=1,byrow=TRUE)
l   <- layout(mat,width=c(5,1))
mfuzz.plot2(yeastF,cl=cl,mfrow=NA,colo="fancy", ax.col="red",bg = "black",col.axis="red",col.lab="white",
col.main="green",col.sub="blue",col="blue",cex.main=2, single=3,x11=FALSE)

mfuzzColorBar(col="fancy",main="Membership",cex.main=1)

}
}
\keyword{hplot}
