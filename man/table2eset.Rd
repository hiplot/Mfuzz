\name{table2eset}
\alias{table2eset} 
\title{Conversion of table to Expression set object.}
\description{A expression matrix stored as a table (in
  a defined format) is read and converted to Expression Set object.}
\usage{table2eset(filename)}

\arguments{\item{filename}{name of file to be scanned in}
  }

  \details{The expression matrix stored as table in the file
    has to follow some conventions in order to be able to be converted
    to an \code{Expression Set} object: The first row of the file contains sample labels and optionally, the second column can
contains the time points. If the second row is used for the input the time, the first field
in the second row must contain \dQuote{Time}. Similarly, the first column
contains unique gene IDs and optionally second row can contain gene names. If the second row contains gene
names, the second field in the first row must contain \dQuote{Gene.Name}. The rest of the file
contains expression data.  As example, two tables with expression data are provided.
These examples can be viewed by inputing \code{data(yeast.table)} and \code{data(yeast.table2)}
in the R console.}
  
\value{An \code{Expression Set} object is generated.}

\author{Matthias E. Futschik  (\url{http://www.sysbiolab.eu})}


\keyword{utilities}
