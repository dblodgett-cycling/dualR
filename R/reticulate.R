.data <- NULL
datatable.aware = TRUE 

#' Read Fit File
#' @param f path to fit file
#' @export
#' @examples 
#' read_fit_file(system.file("fit/fit1.fit", package = "dualR"))
read_fit_file <- function(f) {
  
  if(Sys.getenv("FITCSVTOOL") != "") {
    return(read_fit_file_sdk(f))
  }
  
  sweat <- reticulate::import("sweat", convert = FALSE)
  d <- sweat$read_fit(f)
  d <- d$reset_index()
  reticulate::py_to_r(d)
}

#' @importFrom data.table fread
read_fit_file_sdk <- function(f, overwrite = TRUE) {
  out_f <- gsub("\\.fit", "_data.csv", f)
  out_f_2 <- gsub("\\.fit", ".csv", f)
  
  on.exit(unlink(c(out_f, out_f_2)))
  
  fitcsvjar <- Sys.getenv("FITCSVTOOL")

  system2("java", c("-jar", fitcsvjar, "--defn none", "--data record", f))

  record <- clean_table(fread(out_f, fill = TRUE))
  
  fit_time_origin <- as.POSIXct("1989-12-31 00:00:00", tz = "GMT")
  
  record$timestamp <- as.POSIXct(record$timestamp, tz = "GMT", 
                                 origin = fit_time_origin)
}

#' Get Fit Metadata
#' @param f path to fit file
#' @export
#' @examples
#' get_fit_meta(system.file("fit/fit2.fit", package = "dualR"))
get_fit_meta <- function(f) {
  
  if(Sys.getenv("FITCSVTOOL") != "") {
    return(get_fit_meta_sdk(f))
  }
  
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

get_fit_meta_sdk <- function(f) {
  out_f <- gsub("\\.fit", "_data.csv", f)
  out_f_2 <- gsub("\\.fit", ".csv", f)
  
  on.exit(unlink(c(out_f, out_f_2)))
  
  fitcsvjar <- Sys.getenv("FITCSVTOOL")
  
  system2("java", c("-jar", fitcsvjar, "--defn none", "--data file_id", f))
  
  clean_table(fread(out_f, fill = TRUE))
  
}

clean_table <- function(x) {
  names(x) <- gsub("\\[.*\\]", "", gsub(".*\\.", "", names(x)))
  
  if(any(!(fltr <- !grepl("^V", names(x))))) {
    x <- x[, fltr, with = FALSE]
  }
  
  as.data.frame(x)
}

#' Get Device Metadata
#' @param f path to fit file
#' @export
#' @examples 
#' get_device_meta(system.file("fit/zwift/wahoo_h3.fit", package = "dualR"))
get_device_meta <- function(f) {
  
  if(Sys.getenv("FITCSVTOOL") != "") {
    return(get_device_meta_sdk(f))
  }
  
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

get_device_meta_sdk <- function(f) {
  out_f <- gsub("\\.fit", "_data.csv", f)
  out_f_2 <- gsub("\\.fit", ".csv", f)
  
  on.exit(unlink(c(out_f, out_f_2)))
  
  fitcsvjar <- Sys.getenv("FITCSVTOOL")
  
  system2("java", c("-jar", fitcsvjar, "--defn none", "--data device_info", f))
  
  clean_device_info(clean_table(fread(out_f, fill = TRUE)))
  
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
