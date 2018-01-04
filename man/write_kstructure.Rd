\encoding{UTF-8}
\name{write_kstructure}
\alias{write_kstructure}
\title{Write a knowledge structure file}

\description{
  Write a knowledge structure to
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  write_kstructure(x, filename, format = "SRBT")
}

\arguments{
  \item{x}{The data to be written, either a binary matrix or an object
    of \code{kstructure} class.}
  \item{filename}{A character string specifying the name of the base file.}

  \item{format}{Specification of the files format. Can be "SRBT" (default), "KST", or
    "matrix".
  }
}

\details{
  The \code{format} values \code{"SRBT"}, \code{"KST"}, and 
  \code{"matrix"} refer to the different generations of file formats 
  described in \link{kstIO-package}.  
}

\references{
  Hockemeyer, C. (2001). \emph{KST Tools User Manual} (2nd ed.). 
  \url{https://kst.hockemeyer.at/techreports/KST-Tools_TechRep_FWF01.pdf}.

  Pötzi, S. \& Wesiak, G. (2001). \emph{SRbT Tools User Manual.}
  \url{https://kst.hockemeyer.at/techreports/SRBT-Tools_TechRep_FWF01.pdf}
}

\author{Cord Hockemeyer \email{cord.hockemeyer@uni-graz.at}}

\seealso{\code{\link[kst]{kstructure}},
	 \link{kstIO-package}
}

\examples{
# Obtain data to write from the 'pks' package
library(kst)
data(DoignonFalmagne7)
kst <- kstructure(as.pattern(DoignonFalmagne7$K, as.set=TRUE))
# Write structure to file
write_kstructure(kst, "DF7.struct") # Write in (default) SRBT format
# Write the matrix directly in (old) KST format
write_kstructure(DoignonFalmagne7$K, "DF7.struct", "KST") 
}

\keyword{file}
