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
  
}

#' @importFrom data.table fread
read_fit_file_sdk <- function(f, overwrite = TRUE) {
  out_f <- gsub("\\.fit", "_data.csv", f)
  out_f_2 <- gsub("\\.fit", ".csv", f)
  
  on.exit(unlink(c(out_f, out_f_2)))
  
  fitcsvjar <- Sys.getenv("FITCSVTOOL")

  system2("java", c("-jar", fitcsvjar, "--defn none", "--data record", f), 
          stdout = FALSE)

  record <- clean_table(out_f)
  
  fit_time_origin <- as.POSIXct("1989-12-31 00:00:00", tz = "GMT")
  
  record$timestamp <- as.POSIXct(record$timestamp, tz = "GMT", 
                                 origin = fit_time_origin)
  
  record <- dplyr::rename(record, datetime = timestamp)
  
  record
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
  
}

get_fit_meta_sdk <- function(f) {
  out_f <- gsub("\\.fit", "_data.csv", f)
  out_f_2 <- gsub("\\.fit", ".csv", f)
  
  on.exit(unlink(c(out_f, out_f_2)))
  
  fitcsvjar <- Sys.getenv("FITCSVTOOL")
  
  system2("java", c("-jar", fitcsvjar, "-e --data file_id", f), 
          stdout = FALSE)
  
  clean_table(out_f_2)
  
}

get_row <- function(row) {
  ns <- row[seq(4, length(row), 3)]
  
  filt <- as.logical(ns != "" & !is.na(ns))
  
  ns <- ns[filt]
  
  vl <- row[seq(5, length(row), 3)]
  
  suppressWarnings(vl[!is.na(vl) & (as.numeric(vl) == 1 | vl == "")] <- NA)
  
  if(all(is.na(vl))) return(NULL)
  
  vl <- as.list(tolower(vl[filt]))
  
  names(vl) <- ns
  
  data.frame(vl)
}

clean_table <- function(x) {
  x <- try(fread(x, fill = TRUE, integer64 = "character"), silent = TRUE)
  
  if(inherits(x, "try-error")) return(data.frame())
  
  if(names(x)[1] == "Type") {
    x <- dplyr::bind_rows(apply(x, 1, get_row))
    
    if("serial_number" %in% names(x)) {
      x <- dplyr::filter(x, !is.na(.data$serial_number))
    }
    
    if("time_created" %in% names(x)) {
      fit_time_origin <- as.POSIXct("1989-12-31 00:00:00", tz = "GMT")
      
      x$time_created <- as.POSIXct(as.integer(x$time_created), tz = "GMT", 
                                   origin = fit_time_origin)
    }
  }
  
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
  
}

get_device_meta_sdk <- function(f) {
  out_f <- gsub("\\.fit", "_data.csv", f)
  out_f_2 <- gsub("\\.fit", ".csv", f)
  
  on.exit(unlink(c(out_f, out_f_2)))
  
  fitcsvjar <- Sys.getenv("FITCSVTOOL")
  
  system2("java", c("-jar", fitcsvjar, "-e", "--defn none --data device_info", f), 
          stdout = FALSE)
  
  clean_device_info(clean_table(out_f_2))
  
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
