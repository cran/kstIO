\encoding{UTF-8}
\name{write_surmiserelation}
\alias{write_surmiserelation}
\title{Write a (surmise) relation file}

\description{
  Write a surmise relation to
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  write_surmiserelation(x, filename, format = "SRBT")
}

\arguments{
  \item{x}{The data to be written, either a quadratic binary matrix or an object
    of \code{relation} class.}
  \item{filename}{A character string specifying the name of the base file.}

  \item{format}{Specification of the files format. Can be "SRBT" (default), "CSV", or
    "matrix".
  }
}

\details{
  The \code{format} values \code{"SRBT"} and 
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

\seealso{\link{kstIO-package}}

\examples{
# Obtain data to write from the 'pks' package
library(kst)
library(pks)
d <- getwd()
setwd(tempdir())
data(DoignonFalmagne7)
r <- as.relation(kspace(kstructure(as.pattern(DoignonFalmagne7$K, as.set=TRUE))))
# Write surmise relation to file
write_surmiserelation(r, "DF7.bas") # Write in (default) SRBT format
write_surmiserelation(r, "DF7.bas", "matrix") # matrix format
setwd(d)
}

\keyword{file}
