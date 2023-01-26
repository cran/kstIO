###
### write_kdata.R
###
### dependencies: pks, kst, MASS
###

write_kdata <- function (x, filename, format="SRBT") {
  
  if (!inherits(x, "matrix")) 
    stop(sprintf("%s must be of class %s!",
                 dQuote("x"),
                 dQuote("matrix")
    ))
  
  rownames(x) <- NULL
  colnames(x) <- NULL
  
  if (format == "CSV" ) {
    write.csv(x, filename, row.names = FALSE)
  } else {
    con <- file(filename)
    if (is.null(con))
      stop(sprintf("Unable to open file %s.", dQuote(filename)))
    open(con, open="w")
    
    size <- dim(x)
    
    if (format == "SRBT")
      cat("#SRBT v2.0 data ASCII\n", file=con)
    
    if ((format == "SRBT") | (format == "KST")) {
      cat(sprintf("%d\n", size[2]), file=con)
      cat(sprintf("%d\n", size[1]), file=con)
    }
    
    write.matrix(x, sep="", file=con)
    
    close(con)
  }
}
