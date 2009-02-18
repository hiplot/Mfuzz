\name{yeast.table}

\docType{data}

\alias{yeast.table}

\title{Gene expression data of the yeast cell cycle as table}

\description{The data serves as an example for the format required for
  uploading tables with expression data into \emph{Mfuzzgui.} The first
  row contains the names of the samples, the second row contains the
  measured time points. Note that  \dQuote{TIME} has to
  placed in the first field of the second row.

  The first column contains unique identifiers for genes; optionally the
  second row can contain gene names if \dQuote{GENE.NAMES} is in
  the second field in the first row.

  An example for an table without optional fields is the dataset
  \code{\link[Mfuzz]{yeast.table2}}.
  
  The exemplary tables can be found in the data sub-folder of the 
 \emph{Mfuzzgui} package. 
}

%\usage{data(yeast.table)}

\references{Cho et al., A genome-wide transcriptional analysis of the
  mitotic cell cycle, Mol Cell. 1998 Jul;2(1):65-73.}


\seealso{\code{\link[Mfuzz]{yeast.table2}} }

\keyword{datasets}


