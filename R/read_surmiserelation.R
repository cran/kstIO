###
### read_surmiserelation.R
###
### dependencies: pks, stringr, relations
###

read_surmiserelation <- function (filename, format="auto", as.letters=TRUE, close=FALSE) {

  f <- readLines(con=filename)
  if (length(f) == 0) {
    stop(sprintf("Unable to read file %s!", filename))
  }
  
  if (format == "SRBT") {
    p <- str_locate(f[1], "#SRBT v2.0 relation")
    if (is.na(p[1][1]) | p[1][1] != 1)
      stop(sprintf("File %s has no correct SRBT header.", filename))
    noi <- as.numeric(f[2])
    if (noi <= 0)
      stop(sprintf("Invalid number of items in %s.", filename))
    offset <- 2
    p <- str_locate(f[3], "#")
    while (!is.na(p[1][1])) {
      offset <- offset + 1
      p <- str_locate(f[offset+1], "#")
    }
  }
  else if (format == "matrix") {
    noi <- str_length(f[1])
    offset <- 0
  }
  else {   # format == "auto"
    p <- str_locate(f[1], "#SRBT")
    if (!is.na(p[1][1]) & p[1][1] == 1) {
      p <- str_locate(f[1], "#SRBT v2.0 relation")
      if (p[1][1] != 1)
        stop(sprintf("File %s has no correct SRBT header.", filename))
      noi <- as.numeric(f[2])
      if (noi <= 0)
        stop(sprintf("Invalid number of items in %s.", filename))
      offset <- 2
      p <- str_locate(f[3], "#")
      while (!is.na(p[1][1])) {
        offset <- offset + 1
        p <- str_locate(f[offset+1], "#")
      }
    }
    else { # most probably matrix
      noi <- str_length(f[1])
      offset <- 0
    }
  }   # end of automatic format detection


  mat <- mat.or.vec(noi, noi)
  for (i in 1:noi) {
    mat[i,]<- 1L*as.logical(as.integer(unlist(strsplit(trimws(f[i+offset],which="both"),""))))
  }
  storage.mode(mat) <- "integer"
  if (as.letters) {
    names <- make.unique(letters[(0L:(ncol(mat)-1)) %% 26 + 1])
  } else {
    names <- as.integer(1L:ncol(mat))
  }
  colnames(mat) <- names
  rownames(mat) <- names
  
  rel <- relation(incidence = mat)
  if (close) {
    rel <- transitive_closure(reflexive_closure(rel))
    mat <- relation_incidence(rel)
  }

  
  list(relation = rel, matrix = mat)  
}
