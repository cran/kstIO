###
### write_kbase.R
###
### dependencies: pks, kst, MASS
###

write_kbase <- function (x, filename, format="SRBT") {
  
  if (inherits(x, "kbase"))
    mat <- as.binmat(x)
  else if (!inherits(x, "matrix")) 
    stop(sprintf("%s must be of class %s or %s!",
                 dQuote("x"),
                 dQuote("kbase"),
                 dQuote("matrix")
    ))
  else {
    x <- as.pattern(x, as.set = TRUE)
    class(x) <- c("kbase", class(x))
    mat <- as.binmat(x)
  }
  
  rownames(mat) <- NULL
  colnames(mat) <- NULL
  mat <- 2*mat
  
  dom <- as.set(unique(unlist(as.list(x))))
  ### compute atoms
  y <- as.list(x)
  atoms <- list()
  items <- as.set(lapply(dom, as.character))
  for (i in items) {
    states <- y[which(sapply(y, function(j) grep(i,j))!=0)]
    atom <- set()
    for (j in seq_along(states)) {
      subsets <- lapply(states[-j],set_is_subset, states[[j]])
      if (!any(unlist(subsets))) {
        atom <- c(atom, set(as.set(states[[j]])))
      }
    }
    atoms[[i]] <- atom
  }
  names(atoms) <- unlist(items)
  sind <- 1
  for (s in x) {
    qind <- 1
    for (q in dom) {
      if (s %e% atoms[[qind]])
        mat[sind,qind] <- 1
      qind <- qind + 1
    }
    sind <- sind + 1
  }
  
  con <- file(filename)
  if (is.null(con))
    stop(sprintf("Unable to open file %s.", dQuote(filename)))
  open(con, open="w")
  
  size <- dim(mat)
  
  if (format == "CSV") {
    write.csv(mat, filename, row.names = FALSE)
  } else {
    if (format == "SRBT")
      cat("#SRBT v2.0 basis\n", file=con)
    
    if ((format == "SRBT") | (format == "KST")) {
      cat(sprintf("%d\n", size[2]), file=con)
      cat(sprintf("%d\n", size[1]), file=con)
    }
    else {
      close(con)
      unlink(filename)
      stop(sprintf("Base file must have format %s or %s.",
                   dQuote("SRBT"),
                   dQuote("KST")
      ))
    }
    
    colnames(mat) <- NULL
    write.matrix(mat, sep="", file=con)
    
    close(con)
  }
}
