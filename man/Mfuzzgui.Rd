\name{Mfuzzgui}
\alias{Mfuzzgui}
\title{Graphical user interface for Mfuzz package}
\description{The function \code{Mfuzzgui} provides a graphical user interface for
clustering of microarray data and visualisation of results.
It is based on the functions of the Mfuzz package. 
}
\usage{
Mfuzzgui()
}
\details{The function \code{Mfuzzgui} launches a graphical user interface for the \emph{Mfuzz} package.
It is based on Tk widgets using the R TclTk interface by Peter Dalgaard. It also employs
some pre-made widgets from the tkWidgets Bioconductor-package by Jianhua Zhang for
the selection of objects/files to be loaded. 

Mfuzzgui provides a convenient interface to most functions of the Mfuzz package without restriction 
of flexibility. An exception is the batch processes such as \code{partcoeff} and \code{cselection} routines which 
are used for parameter selection in fuzzy c-means clustering of microarray data. These routines are 
not included in Mfuzzgui. To select various parameters, the underlying Mfuzz routines may be applied.

Usage of Mfuzzgui does not require assumes an pre-built \code{exprSet} object
but can be used with tab-delimited text files containing the gene expression data. 
 Note, however, that the clustering is based on the  
the ordering of samples (arrays) as
of the columns in the expression matrix of the \code{exprSet} object or in the uploaded table, respectively. 
Also, replicated arrays in the 
expression matrix (or table)  are treated as independent by the \texttt{mfuzz} function
and, thus, should be averagered prior to clustering.


For a overview of the functionality of Mfuzzgui, please refer to the package vignette. For a description
of the underlying functions, please refer to the \code{Mfuzz} package. 
}
\value{Mfuzzgui returns a tclObj object.}

\references{

\enumerate{
\item M.E. Futschik and B. Charlisle, Noise robust clustering of gene expression time-course data, \strong{Journal of Bioinformatics 
and Computational Biology}, Vol. 3, No. 4, 965-988, 2005.
\item Cho RJ, Campbell MJ, Winzeler EA,  Steinmetz L, Conway A, Wodicka L, Wolfsberg TG,  Gabrielian  AE, Landsman D, Lockhart  
DJ, Davis RW,  A genome-wide transcriptional analysis of the mitotic cell cycle, \strong{Mol Cell},(2):65-73, 1998.
\item  Mfuzz web-page: \url{http://itb.biologie.hu-berlin.de/~futschik/software/R/Mfuzz}}}

\author{ Matthias E. Futschik
(\url{http://itb.biologie.hu-berlin.de/~futschik})and Lokesh Kumar}
\note{ The newest versions of \code{Mfuzzgui} can be found at the Mfuzz webpage 
 (\url{http://itb.biologie.hu-berlin.de/~futschik/software/R/Mfuzz}).}

\seealso{\code{\link[Mfuzz]{mfuzz}} }

\keyword{misc }
\keyword{cluster}
\keyword{ts}
