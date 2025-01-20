\encoding{UTF-8}
\name{write_kspace}
\alias{write_kspace}
\title{Write a knowledge space file}

\description{
  Write a knowledge space to
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  write_kspace(x, filename, format = "SRBT")
}

\arguments{
  \item{x}{The data to be written, either a binary matrix or an object
    of \code{kspace} class.}
  \item{filename}{A character string specifying the name of the base file.}

  \item{format}{Specification of the files format. Can be "SRBT" (default), "KST", "CSV", or
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
  \url{https://resources.cord-hockemeyer.info/techreports/KST-Tools_TechRep_FWF01.pdf}.

  Poetzi, S. & Wesiak, G. (2001). \emph{SRbT Tools User Manual.}
  \url{https://resources.cord-hockemeyer.info/techreports/SRBT-Tools_TechRep_FWF01.pdf}
}

\author{Cord Hockemeyer \email{cord.hockemeyer@uni-graz.at}}

\seealso{\code{\link[kst]{space_property}},
	 \link{kstIO-package}
}

\examples{
# Obtain data to write from the 'pks' package
library(kst)
d <- getwd()
setwd(tempdir())
data(DoignonFalmagne7)
ksp <- kspace(kstructure(as.pattern(DoignonFalmagne7$K, as.set=TRUE)))
# Write space to file
write_kspace(ksp, "DF7.spc") # Write in (default) SRBT format
write_kspace(DoignonFalmagne7$K, "DF7.spc", "KST") # Write the matrix directly in (old) KST format
setwd(d)
}

\keyword{file}
