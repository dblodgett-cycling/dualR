.data <- NULL

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

#' Get Device Metadata
#' @param f path to fit file
#' @export
#' @examples 
#' get_device_meta(system.file("fit/zwift/wahoo_h3.fit", package = "dualR"))
get_device_meta <- function(f) {
  fitparse <- reticulate::import("fitparse")
  
  fitfile <- fitparse$FitFile(f)
  
  out = list()
  
  i <- 1
  
  for(record in reticulate::iterate(fitfile$get_messages("device_info"))) {
  
    out[[i]] <- list()
    
    for(r in reticulate::iterate(record)) {
      out[[i]][r$name] = list(as.character(r$value))
    }
    
    out[[i]][lengths(out[[i]]) == 0] <- ""
    
    out[[i]] <- data.frame(out[[i]])
    
    i <- i + 1
    
  }
  
  clean_device_info(dplyr::bind_rows(out))
  
}

#' @importFrom dplyr `%>%`
clean_device_info <- function(out) {
  good_names <- c("manufacturer", "product", "product_name", "serial_number", 
                  "timestamp", "ant_device_number", "antplus_device_type", 
                  "device_type", "source_type", "software_version")
  
  out <- out[, names(out) %in% good_names]
  
  if(nrow(out) > 0) {
    
    out[["id_temp"]] <- apply(dplyr::select(out, -.data$timestamp), 1, 
                              function(x) do.call(paste, as.list(x)))
    
    unq <- dplyr::distinct(dplyr::select(out, -.data$timestamp))
    
    unq[["id"]] <- seq_len(nrow(unq))
    
    dplyr::select(unq, .data$id_temp, .data$id) %>%
      dplyr::right_join(out, by = "id_temp") %>%
      dplyr::select(-.data$id_temp) %>%
      dplyr::group_by(.data$id)%>%
      dplyr::filter(.data$timestamp == min(.data$timestamp)) %>%
      dplyr::ungroup()
    
  } else {
    
    out
    
  }
  
}
