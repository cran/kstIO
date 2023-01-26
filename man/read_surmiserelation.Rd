\encoding{UTF-8}
\name{read_surmiserelation}
\alias{read_surmiserelation}
\title{Read surmise relation file}

\description{
  Read a surmise relation from
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  read_surmiserelation(filename, format = "auto", as.letters = TRUE, close = FALSE)
}

\arguments{
  \item{filename}{A character string specifying the name of the base file.}

  \item{format}{Specification of the files format. Can be "SRBT", 
    "matrix", "CSV", or "auto" (default).}
  \item{as.letters}{logical, should the elements of the sets be letters
                    or numbers? Defaults to TRUE.}
  \item{close}{logical, should the relation be closed under reflexivity
	  and transitivity? Defaults to FALSE}
}

\details{
  The \code{format} values \code{"SRBT"} and 
  \code{"matrix"} refer to the different generations of file formats 
  described in \link{kstIO-package}.  The value \code{"auto"} (default)
  requests an automatic detection of the format by the \code{read_XXX}
  function. 

  If \code{as.letters} is \code{TRUE} (default), the elements of the sets are letters,
  otherwise numbers.

  If \code{close} is \code{TRUE} (default is \code{FALSE}), the relation is closed under
  reflexivity and transitivity, otherwise it is returned as is.
}

\value{A list with two elements:
	\item{relation}{The surmise relation as object of class \code{relation}.}
	\item{matrix}{The incidence matrix of the surmise relation.}
}

\references{
  Hockemeyer, C. (2001). \emph{KST Tools User Manual} (2nd ed.). 
  \url{https://kst.hockemeyer.at/techreports/KST-Tools_TechRep_FWF01.pdf}.

  Poetzi, S. & Wesiak, G. (2001). SRbT Tools User Manual.
  \url{https://kst.hockemeyer.at/techreports/SRBT-Tools_TechRep_FWF01.pdf}
}


\author{Cord Hockemeyer \email{cord.hockemeyer@uni-graz.at}}

\seealso{\link{kstIO-package}}

\examples{
# Produce a relation file
library(kst)
d <- getwd()
setwd(tempdir())
data(DoignonFalmagne7)
r <- as.relation(kspace(kstructure(as.pattern(DoignonFalmagne7$K, as.set=TRUE))))
write_surmiserelation(r, "DF7.rel", "SRBT") # SRBT format
# Read file
read_surmiserelation("DF7.rel") # Automatic format detection
read_surmiserelation("DF7.rel", "SRBT") # Explicit format specification
setwd(d)
}

\keyword{file}
