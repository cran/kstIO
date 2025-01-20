\encoding{UTF-8}
\name{read_kfamset}
\alias{read_kfamset}
\title{Read a family of sets from file}

\description{
  Read a family of sets from
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  read_kfamset(filename, format = "auto", as.letters = TRUE)
}

\arguments{
  \item{filename}{A character string specifying the name of the space file.}

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
  
  \code{read_kfamset()} reads any knowledge space file (space, structure,
  basis) and ignores any file type infor in SRBT file headers.

  If \code{as.letters} is \code{TRUE} the elements of the sets are letters,
  otherwise numbers. 
}

\value{A list with the following elements:
  \item{matrix}{the read structure/data as binary matrix}
  \item{sets}{the read structure as \code{kspace}.}
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

  There exists no \code{write_kfamset} function on purpose.
}

\author{Cord Hockemeyer \email{cord.hockemeyer@uni-graz.at}}

\seealso{\code{\link[kst]{space_property}},
	 \link{kstIO-package}
}

\examples{
# Produce a space file
library(kst)
d <- getwd()
setwd(tempdir())
data(DoignonFalmagne7)
ksp <- kspace(kstructure(as.pattern(DoignonFalmagne7$K, as.set=TRUE)))
write_kspace(ksp, "DF7.spc") # Write in (default) SRBT format
# Read file
read_kfamset("DF7.spc") # Automatic format detection (default)
read_kfamset("DF7.spc", "SRBT") # Explicit format specification
setwd(d)
}

\keyword{file}
