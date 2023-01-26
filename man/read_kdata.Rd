\encoding{UTF-8}
\name{read_kdata}
\alias{read_kdata}
\title{Read a response patterns file}

\description{
  Read a set of response patterns from
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  read_kdata(filename, format = "auto", as.letters = TRUE)
}

\arguments{
  \item{filename}{A character string specifying the name of the data file.}

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

\value{A binary matrix with the response patterns.}

\references{
  Hockemeyer, C. (2001). \emph{KST Tools User Manual} (2nd ed.). 
  \url{https://kst.hockemeyer.at/techreports/KST-Tools_TechRep_FWF01.pdf}.

  Poetzi, S. & Wesiak, G. (2001). SRbT Tools User Manual.
  \url{https://kst.hockemeyer.at/techreports/SRBT-Tools_TechRep_FWF01.pdf}
}

\note{In automatic format detection, the distinction between \code{"matrix"}
  and \code{"KST"} formats work somewhat heuristic. In other words, in rare
  cases the automatic detection might give the wrong result.
}

\author{Cord Hockemeyer \email{cord.hockemeyer@uni-graz.at}}

\seealso{\link{kstIO-package}
}

\examples{
# Produce a data file
d <- getwd()
setwd(tempdir())
data(DoignonFalmagne7)
df <- as.binmat(DoignonFalmagne7$N.R)
write_kdata(df, "DF7.dat", "matrix") # matrix format (without any headers)
# Read file
read_kdata("DF7.dat") # Automatic format detection (default)
read_kdata("DF7.dat", "matrix") # Explicit ormat specification
setwd(d)
}

\keyword{file}
