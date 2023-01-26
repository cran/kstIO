\encoding{UTF-8}
\name{write_surmisefunction}
\alias{write_surmisefunction}
\title{Write a surmise function file}

\description{
  Write a surmise function to
  a file. The file formats are described in the \link{kstIO-package}
  information page.
}

\usage{
  write_surmisefunction(x, filename)
}

\arguments{
  \item{x}{The data to be written, either a quadratic binary matrix or an object
    of \code{relation} class.}
  \item{filename}{A character string specifying the name of the base file.}
}

\details{
  SUrmise function can (so far) be stored only in CSV format.
}

\author{Cord Hockemeyer \email{cord.hockemeyer@uni-graz.at}}

\seealso{\link{kstIO-package}}


\keyword{file}
