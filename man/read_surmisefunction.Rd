\encoding{UTF-8}
\name{read_surmisefunction}
\alias{read_surmisefunction}
\title{Read surmise function file}

\description{
  Read a surmise function from
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  read_surmisefunction(filename)
}

\arguments{
  \item{filename}{A character string specifying the name of the base file.}
}

\details{
  Surmise function files exist (so far) only in CSV format.
}

\value{A list with two elements:
	\item{relation}{The surmise relation as object of class \code{relation}.}
	\item{matrix}{The incidence matrix of the surmise relation.}
}


\author{Cord Hockemeyer \email{cord.hockemeyer@uni-graz.at}}

\seealso{\link{kstIO-package}}

 
\keyword{file}
