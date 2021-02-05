#' Read Fit File
#' @param f path to fit file
#' @export
#' @examples 
#' read_fit_file(system.file("fit/fit1.fit", package = "dualR"))
read_fit_file <- function(f) {
  sweat <- reticulate::import("sweat", convert = FALSE)
  d <- sweat$read_fit(f)
  d <- d$reset_index()
  reticulate::py_to_r(d)
}

#' Get Fit Metadata
#' @param f path to fit file
#' @export
#' @examples
#' get_fit_meta(system.file("fit/fit2.fit", package = "dualR"))
get_fit_meta <- function(f) {
  fitparse <- reticulate::import("fitparse")
  
  fitfile <- fitparse$FitFile(f)
  
  out = list()
  
  for(record in reticulate::iterate(fitfile$get_messages("file_id"))) {
    for(r in reticulate::iterate(record)) {
      out[r$name] = list(r$value)
    }
  }
  
  out
  
}
