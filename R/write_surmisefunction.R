###
### write_surmisefunction.R
###

write_surmisefunction <- function (x, filename) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s or %s!",
                 dQuote("x"),
                 dQuote("relation"),
                 dQuote("matrix")
    ))
  }
  
  write.csv(x, filename, row.names = FALSE)
}
  