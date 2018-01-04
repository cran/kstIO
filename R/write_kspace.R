###
### write_kspace.R
###
### dependencies: pks, kst, MASS
###

write_kspace <- function (x, filename, format="SRBT") {

  if (inherits(x, "kspace")) {
    x <- as.binmat(x)
  }

  if (!inherits(x, "matrix")) 
    stop(sprintf("%s must be of class %s or %s!",
                 dQuote("x"),
		 dQuote("kspace"),
		 dQuote("matrix")
    ))

  rownames(x) <- NULL
  colnames(x) <- NULL

  con <- file(filename)
  if (is.null(con))
    stop(sprintf("Unable to open file %s.", dQuote(filename)))
  open(con, open="w")

  size <- dim(x)
  
  if (format == "SRBT")
    cat("#SRBT v2.0 space ASCII\n", file=con)

  if ((format == "SRBT") | (format == "KST")) {
    cat(sprintf("%d\n", size[2]), file=con)
    cat(sprintf("%d\n", size[1]), file=con)
  }

  write.matrix(x, sep="", file=con)

  close(con)
}
