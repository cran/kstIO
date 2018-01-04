\encoding{UTF-8}
\name{write_kdata}
\alias{write_kdata}
\title{Write a knowledge space theory file}

\description{
  Write a data set to
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  write_kdata(x, filename, format = "SRBT")
}

\arguments{
  \item{x}{The data to be written, as a binary matrix.}
  \item{filename}{A character string specifying the name of the data file.}

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

\seealso{\link{kstIO-package}
}

\examples{
# Obtain data to write from the 'pks' package
data(DoignonFalmagne7)
d <- as.binmat(DoignonFalmagne7$N.R)
# Write data to file
write_kdata(d, "DF7.dat") # Write in (default) SRBT format
write_kdata(DoignonFalmagne7$K, "DF7.dat", "matrix") # matrix format (without any headers)
}

\keyword{file}
