.data <- NULL
datatable.aware = TRUE 

#' Read Fit File
#' @param f path to fit file
#' @export
#' @importFrom FITfileR readFitFile records
#' @importFrom data.table rbindlist setnames
#' @examples 
#' read_fit_file(system.file("fit/fit1.fit", package = "dualR"))
read_fit_file <- function(f) {
  
  if(!inherits(f, "FitFile"))
    f <- readFitFile(f)
  
  f <- records(f)
  
  if(!inherits(f, "data.frame"))
    f <- rbindlist(f, use.names = TRUE, fill = TRUE)
  
  f <- setnames(f, old = "timestamp", new = "datetime")
  
  f <- f[order(f$datetime),]
  
  return(f)
  
}

#' Get Fit Metadata
#' @param f path to fit file of FitFile object
#' @export
#' @importFrom FITfileR getMessagesByType
#' @examples
#' f <- FITfileR::readFitFile(system.file("fit/fit1.fit", package = "dualR"))
#' get_fit_meta(f)
#' 
get_fit_meta <- function(f) {
  
  if(!inherits(f, "FitFile"))
    f <- readFitFile(f)
  
  return(getMessagesByType(f, "file_id") |>
           as.data.frame())
  
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

#' @importFrom data.table fread
clean_table <- function(x) {
  if(inherits(x, "character")) {
    
    x <- try(fread(x, fill = TRUE, integer64 = "character"), silent = TRUE)
    
    if(inherits(x, "try-error")) return(data.frame())
  }
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
#' @param d path to fit file or object of class FitFile
#' @export
#' @examples 
#' get_device_meta(system.file("fit/zwift/wahoo_h3.fit", package = "dualR"))
get_device_meta <- function(d) {
  
  if(!inherits(d, "FitFile"))
    d <- readFitFile(d)
  
  d <- try(getMessagesByType(d, "device_info"), silent = TRUE)
  
  if(inherits(d, "try-error")) return(data.frame())
  
  if(!inherits(d, "data.frame")) {
    
    types <- lapply(d, function(x) sapply(x, class))
    types <- dplyr::bind_rows(types)
    types <- sapply(names(types), function(x) {
      x <- types[[x]]
      x <- x[!is.na(x)]
      if(any(x == "POSIXct")) return("POSIXct")
      if(any(x == "character")) return("character")
      if(any(x == "integer")) return("integer")
      "numeric"
    })
    
    d <- lapply(d, function(x) {
      for(y in names(x)) {
        x[[y]] <- methods::as(x[[y]], types[names(types) == y])
      }
      x
    })
    
    d <- dplyr::bind_rows(d)
  }
  
  return(clean_device_info(clean_table(d)))
  
}

#' @importFrom dplyr `%>%`
clean_device_info <- function(out) {
  good_names <- c("manufacturer", "product", "product_name", "serial_number", 
                  "timestamp", "ant_device_number", "antplus_device_type", 
                  "device_type", "source_type", "software_version")
  
  out <- out[, names(out) %in% good_names]
  
  if(nrow(out) > 0) {
    
    out[["id_temp"]] <- apply(dplyr::select(out, -"timestamp"), 1, 
                              function(x) do.call(paste, as.list(x)))
    
    unq <- dplyr::distinct(dplyr::select(out, -"timestamp"))
    
    unq[["id"]] <- seq_len(nrow(unq))
    
    out <- dplyr::select(unq, dplyr::all_of(c("id_temp", "id"))) %>%
      dplyr::right_join(out, by = "id_temp") %>%
      dplyr::select(-id_temp) %>%
      dplyr::group_by(.data$id)%>%
      dplyr::filter(.data$timestamp == min(.data$timestamp)) %>%
      dplyr::filter(dplyr::row_number() == 1) |>
      dplyr::ungroup()
    
    out[sapply(1:nrow(out), function(x) {
      o <- out[x, names(out)[!names(out) %in% c("id", "timestamp")]]
        !all(is.na(o) | o == 0)
      }),]
    
  } else {
    
    out
    
  }
  
}
