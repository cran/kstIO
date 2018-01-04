###
### read_kspace.R
###
### dependencies: pks, stringr
###

read_kspace <- function (filename, format="auto") {

  f <- readLines(con=filename)
  if (length(f) == 0) {
    stop(sprintf("Unable to read file %s!", filename))
  }
  
  if (format == "SRBT") {
    p <- str_locate(f[1], "#SRBT v2.0 space ASCII")
    if (is.na(p[1][1]) | p[1][1] != 1)
      stop(sprintf("File %s has no correct SRBT header.", filename))
    noi <- as.numeric(f[2])
    if (noi <= 0)
      stop(sprintf("Invalid number of items in %s.", filename))
    nos <- as.numeric(f[3])
    if (nos <= 0)
      stop(sprintf("Invalid number of states in %s.", filename))
    offset <- 3
  }
  else if (format == "KST") {
    noi <- as.numeric(f[1])
    if (noi <= 0)
      stop(sprintf("Invalid number of items in %s.", filename))
    nos <- as.numeric(f[2])
    if (nos <= 0)
      stop(sprintf("Invalid number of states in %s.", filename))
    offset <- 2
  }
  else if (format == "matrix") {
    noi <- str_length(f[1])
    nos <- length(f)
    offset <- 0
  }
  else {   # format == "auto"
    p <- str_locate(f[1], "#SRBT")
    if (!is.na(p[1][1]) & p[1][1] == 1) {
      p <- str_locate(f[1], "#SRBT v2.0 space ASCII")
      if (p[1][1] != 1)
        stop(sprintf("File %s has no correct SRBT header.", filename))
      noi <- as.numeric(f[2])
      if (noi <= 0)
        stop(sprintf("Invalid number of items in %s.", filename))
      nos <- as.numeric(f[3])
      if (nos <= 0)
        stop(sprintf("Invalid number of states in %s.", filename))
      offset <- 3
    }
    else if (str_length(f[1]) == str_length(f[length(f)])) { # most probably matrix
      nos <- length(f)
      noi <- str_length(f[1])
      offset <- 0
    }
    else {    # most probably KST format
      noi <- as.numeric(f[1])
      if (noi <= 0)
        stop(sprintf("Invalid number of items in %s.", filename))
      nos <- as.numeric(f[2])
      if (nos <= 0)
        stop(sprintf("Invalid number of states in %s.", filename))
      offset <- 2
    }
  }   # end of automatic format detection

  mat <- mat.or.vec(nos, noi)
  for (i in 1:nos) {
    mat[i,]<- 1*as.logical(as.numeric(unlist(strsplit(trimws(f[i+offset],which="both"),""))))
  }
  s <- as.pattern(mat, as.set=TRUE)
  class(s) <- c("kspace", "kstructure", class(s))
  
  list(matrix=mat, sets=s)
  
}
