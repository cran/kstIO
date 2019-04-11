###
### write_surmiserelation.R
###
### dependencies: pks, kst, MASS, relation
###

write_surmiserelation <- function (x, filename, format="SRBT") {

  if (inherits(x, "relation")) {
    mat <- relation_incidence(x)
    if (dim(mat)[1] != dim(mat)[2]) {
      stop(sprintf("%s must be a binary relation on one set!",
                   dQuote("x")))
    }
  } else if (!inherits(x, "matrix")) 
    stop(sprintf("%s must be of class %s or %s!",
                 dQuote("x"),
		 dQuote("relation"),
		 dQuote("matrix")
    ))
  else {
    mat <- x
    if (dim(mat)[1] != dim(mat)[2]) {
    stop(sprintf("%s must be a quadratic matrix!",
                 dQuote("x")))
    }
  }

  rownames(mat) <- NULL
  colnames(mat) <- NULL

  con <- file(filename)
  if (is.null(con))
    stop(sprintf("Unable to open file %s.", dQuote(filename)))
  open(con, open="w")

  size <- dim(mat)
  
  if (format == "SRBT") {
    cat("#SRBT v2.0 relation\n", file=con)
    cat(sprintf("%d\n", size[1]), file=con)
  } else if (format != "matrix") {
    stop(sprintf("%s must be either %s or %s!",
                 dQuote("format"),
                 dQuote("SRBT"),
                 dQuote("matrix")))
  }

  write.matrix(mat, sep="", file=con)

  close(con)
}
