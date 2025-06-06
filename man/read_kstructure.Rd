\encoding{UTF-8}
\name{read_kstructure}
\alias{read_kstructure}
\title{Read a knowledge structure file}

\description{
  Read a knowledge structure from
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  read_kstructure(filename, format = "auto", as.letters = TRUE)
}

\arguments{
  \item{filename}{A character string specifying the name of the structure file.}

  \item{format}{Specification of the files format. Can be "SRBT", "KST",
    "matrix", "CSV", or "auto" (default).}
  \item{as.letters}{logical, should the elements of the sets be letters
                    or numbers?}
}

\details{
  The \code{format} values \code{"SRBT"}, \code{"KST"}, and 
  \code{"matrix"} refer to the different generations of file formats 
  described in \link{kstIO-package}.  The value \code{"auto"} (default)
  requests an automatic detection of the format by thje \code{read_XXX}
  function. 

  If \code{as.letters} is \code{TRUE} the elements of the sets are letters,
  otherwise numbers.
}

\value{A list with the following elements:
  \item{matrix}{the read structure/data as binary matrix}
  \item{sets}{the read structure as object of class \code{kstructure}.}
}

\references{
  Hockemeyer, C. (2001). \emph{KST Tools User Manual} (2nd ed.). 
  \url{https://resources.cord-hockemeyer.info/techreports/KST-Tools_TechRep_FWF01.pdf}.

  Poetzi, S. & Wesiak, G. (2001). SRbT Tools User Manual.
  \url{https://resources.cord-hockemeyer.info/techreports/SRBT-Tools_TechRep_FWF01.pdf}
}

\note{In automatic format detection, the distinction between \code{"matrix"}
  and \code{"KST"} formats work somewhat heuristic. In other words, in rare
  cases the automatic detection might give the wrong result.
}

\author{Cord Hockemeyer \email{cord.hockemeyer@uni-graz.at}}

\seealso{\code{\link[kst]{kstructure}},
	 \link{kstIO-package}
}

\examples{
# Produce a structure file
library(kst)
d <- getwd()
setwd(tempdir())
data(DoignonFalmagne7)
kst <- kstructure(as.pattern(DoignonFalmagne7$K, as.set=TRUE))
write_kstructure(kst, "DF7.struct") # Write in (default) SRBT format
# Read file
read_kstructure("DF7.struct") # Automatic format detection (default)
read_kstructure("DF7.struct", "SRBT") # Explicit format specification
setwd(d)
}

\keyword{file}
