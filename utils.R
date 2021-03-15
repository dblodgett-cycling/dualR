pretty_fm <- function(fm, l) {

  fm[sapply(fm, is.null)] <- "unknown"

  sprintf("<h4>%s</h4> Data Source: %s <br/> Created: %s",
          l, fm$manufacturer, fm$time_created)
}

check_fit <- function(f) {
  
  if(grepl("zip", f)) {
    out <- unzip(f, list = TRUE)
    out <- out$Name[out$Length == max(out$Length)]
    unzip(f, exdir = dirname(f), files = out)
    f <- file.path(dirname(f), out)
  }
  
  f
  
}

get_dygraph_data <- function(fit, conf) {
  
  tryCatch({
    
    dat <- build_dat(fit$f1$f, conf$f1$label)
    
    if(!is.null(fit$f1_2$f)) {
      dat <- build_dat(fit$f1_2$f, conf$f1_2$label, dat)
    }
    
    if(!is.null(fit$f2$f)) {
      dat <- build_dat(fit$f2$f, conf$f2$label, dat)
    }
    
  },
  error = function(e) {
    showNotification(paste("Error getting timeseries data for plot: \n", e), 
                     type = "error", duration = NULL)
    dat <- NULL
  })
  
  return(dat)

}

build_dat <- function(f, l, dat = NULL) {
  p <- xts(f$power, order.by = f$datetime)
  
  tzone(p) <- "GMT"
  
  if(is.null(dat)) {
    dat <- list(power = setNames(p, l))
  } else {
    dat$power <- cbind(dat$power, setNames(p, l))
  }
  
  if("altitude" %in% names(f)) {
    f <- dplyr::rename(f, elevation = altitude)
  }
  
  if("heart_rate" %in% names(f)) {
    f <- dplyr::rename(f, heartrate = heart_rate)
  }
  
  dat <- add_data(f, "elevation", "Elevation", l, dat)
  
  dat <- add_data(f, "heartrate", "Heart Rate", l, dat)
  
  dat <- add_data(f, "cadence", "Cadence", l, dat)
  
  dat
}

add_data <- function(fit, field, field_name, fit_label, dat) {
  try({
    if(field %in% names(fit)) {
      out <- setNames(xts(fit[[field]], order.by = fit$datetime),
                      paste(field_name, fit_label))
      
      tzone(out) <- "GMT"
      
      if(field %in% names(dat)) {
        dat[[field]] <- cbind(dat[[field]], out)
      } else {
        dat[[field]] <- out
      }
    }})

  dat
}

powercurve_plot <- function(maxes, conf) {
  {

    par(las=2)

    max_1_pch <- 16
    max_1_2_pch <- 1
    max_2_pch <- 17

    # unpacking to match old implementation.
    max_1 <- maxes$m1
    max_2 <- maxes$m2
    max_1_2 <- maxes$m1_2
    
    plot_range <- seq(min(max_1), max(max_1), length.out = length(max_1))

    legend_text <- c(conf$f1$label)
    legend_pch <- max_1_pch
    legend_color <- "black"

    point_col <- rep("blue", length(max_1))

    if(!is.null(max_2)) {
      
      if(!is.null(max_1_2)) {
        legend_text <- c(legend_text, conf$f1_2$label)
        legend_pch <- c(legend_pch, max_1_2_pch)
        legend_color <- c(legend_color, "black")
      }
      
      plot_range <- seq(min(max_1, max_2), max(max_1, max_2), 
                        length.out = length(max_1))

      legend_text <- c(legend_text, conf$f2$label, 
                       "diff < 2%", "2% < diff < 4%", "4% < diff < 8%", "diff > 8%")
      
      legend_pch <- c(legend_pch, max_2_pch, 15, 15, 15, 15)

      perc_diff <- get_perc_diff(max_1, max_2)

      point_col <- ifelse(perc_diff < 0.02, "green",
                          ifelse(perc_diff < 0.04, "yellow",
                                 ifelse(perc_diff < 0.08, "orange", "red")))

      legend_color <- c(legend_color, "black", "green", "yellow", "orange", "red")

    }
  
    plot(plot_range,
         xaxt = "n", ylab = "power (watts)", xlab = NA, col = NA)

    axis(1, at = 1:length(max_1), labels = names(max_1), cex.axis = .9)

    points(max_1, col = point_col, pch = max_1_pch, cex = 2)

    if(!is.null(max_1_2))
      points(max_1_2, col = "black", pch = max_1_2_pch, cex = 2)
    
    if(!is.null(max_2))
      points(max_2, col = point_col, pch = max_2_pch, cex = 2)

    grid(col = "grey")

    legend("topright",
           legend = legend_text,
           pch = legend_pch,
           col = legend_color,
           pt.cex = 2)

  }
}

make_full <- function(x, nms) {
  setNames(c(x, rep(NA, length(nms) - length(x))), nms)
}

get_powercurve_table <- function(maxes, conf) {
  
  lens <- lengths(maxes)
  
  if(!all(lens == max(lens))) {
    
    nms <- names(maxes[[which(lens == max(lens))[1]]])
    
    maxes$m1 <- make_full(maxes$m1, nms)
    maxes$m2 <- make_full(maxes$m2, nms)
    maxes$m1_2 <- make_full(maxes$m1_2, nms)
    
  }
  
  df <- data.frame(p = names(maxes$m1),
                   p1 = as.numeric(maxes$m1))
  digits = 0
  col_names <- conf$f1$label
  
  if(!all(is.na(maxes$m1_2))) {
    df$p1_2 <- as.numeric(maxes$m1_2)
    df$d1_2 <- as.numeric(get_perc_diff(maxes$m1, maxes$m1_2) * 100)
    digits <- c(digits, 0)
    col_names <- c(col_names, conf$f1_2$label, "Verification % Diff")
  }
  
  if(!all(is.na(maxes$m2))) {
    df$p2 <- as.numeric(maxes$m2)
    df$d = as.numeric(get_perc_diff(maxes$m1, maxes$m2) * 100)
    digits <- c(digits, 0, 1)
    col_names <- c(col_names, conf$f2$label, "Validation % Diff")
  }
  
  nms <- df$p
  
  df <- t(dplyr::select(df, -p))
  
  row.names(df) <- col_names
  
  df %>%
    knitr::kable(format = "html", digits = digits, align = "c",
                 col.names = nms, padding = 2) %>%
    kableExtra::kable_styling()
}

get_devices_table <- function(devices) {
  if(nrow(devices) == 0) devices <- data.frame(manufacturer = "undeclared", 
                                               product = "undeclared", 
                                               serial_number = "undeclared")
  
  names(devices) <- gsub("_", " ", names(devices))
  
  knitr::kable(devices, padding = 2, format = "html") %>%
    kableExtra::kable_styling()
}

get_device_summary_table <- function(s) {
  
  s <- lapply(s, function(x) {
    x[sapply(x, is.null)] <- "unknown"
    x
  })
  
  knitr::kable(cbind(as.data.frame(s$fit), as.data.frame(s$power)),
               padding = 2, format = "html") %>%
    kableExtra::add_header_above(c("Data Logger" = 3, "Power Source" = 3)) %>%
    kableExtra::kable_styling()
}

get_dygraph <- function(dat) {
  dygraph(dat, group = "one") %>%
    dyRangeSelector(height = 20) %>%
    dyOptions(useDataTimezone = TRUE) %>%
    dyLegend(show = "always", hideOnMouseOut = FALSE)
}

get_perc_diff <- function(max_1, max_2) {
  suppressWarnings(round((abs(max_1 - max_2) / ((max_1 + max_2) / 2)), 4))
}

#' get configuration
#' @description given app input, return fit file information. 
#' Implements three fit file logic using check_input on each.
#' @param input shiny app input
#' 
get_conf <- function(input) {
  
  temp <- list(f = NULL, label = NULL, offset = NULL)
  
  conf <- list(f1 = temp, f1_2 = temp, f2 = temp)
  
  if(input$demo) {
    
    conf$f1$f <- "inst/fit/rgt/02_rgt_neo.fit"
    conf$f1$label <- "Neo from RGT"
    conf$f1$offset <- 0
    conf$f2$f <- "inst/fit/rgt/02_garmin_quarqd4.fit"
    conf$f2$label <- "QuarqD4 validation from Garmin"
    conf$f2$offset <- -80
    conf$f1_2$f <- "inst/fit/rgt/02_wahoo_neo.fit"
    conf$f1_2$label <- "Neo verification from Wahoo"
    conf$f1_2$offset <- 0
    
  } else {
    
    conf$f1 <- check_input(input$f1, input$f1_label, input$f1_offset, conf$f1)
    
    if(input$extraf1) {
      conf$f1_2 <- check_input(input$f1_2, input$f1_2_label, input$f1_2_offset, conf$f1_2)
    }
    
    if(!is.null(input$f2)) {
      conf$f2 <- check_input(input$f2, input$f2_label, input$f2_offset, conf$f2)
    }
    
    if(length(conf) < 3) return()
    
  }
  
  conf
}

#' check input 
#' @description given fit file descriptors, return configuration object.
#' @param f input file object
#' @param label label for fit file
#' @offset offset for fit file in seconds
#' @conf existing conf (needed?)
#' 
check_input <- function(f, label, offset, conf) {
  if(is.null(f)) {
    
    showNotification("Upload fit file first.", type = "error")
    return()
    
  } else {
    
    if(label == "") {
      showNotification("Provide label for first file.", type = "error")
      return()
    }
    
    conf$f <- check_fit(f$datapath)
    
    conf$label <- label
    
    conf$offset <- offset
    
    return(conf)
  }
}

#' get fit data
#' @description given a three fit configuration file, return data derived from the files.
#' Uses get_fit on all inputs
#' @param conf return from get_conf
#' @param trim logical trim inputs to common time range?
#' 
get_fit_data <- function(conf, trim) {
  
  fit_1 <- get_fit(conf$f1, err = "Error reading fit file 1: \n")
  
  f1_meta <- pretty_fm(get_fit_meta(conf$f1$f), conf$f1$label)
  
  f1_devices <- get_device_meta(conf$f1$f)
  
  f1_device_summary <- get_devices_summary(f1_devices)
  
  if(!is.null(conf$f2$label)) {
    
    fit_2 <- get_fit(conf$f2, err = "Error reading fit file 2: \n")
    
    if(trim) {
      fit_1 <- get_overlapping(fit_1, fit_2)
      fit_2 <- get_overlapping(fit_2, fit_1)
    }
    
    f2_meta <- pretty_fm(get_fit_meta(conf$f2$f), conf$f2$label)
    
    f2_devices <- get_device_meta(conf$f2$f)
    
    f2_device_summary <- get_devices_summary(f2_devices)
    
  } else {
    
    fit_2 <- NULL
    f2_meta <- NULL
    f2_devices <- NULL
    f2_device_summary <- NULL
  }
  
  if(!is.null(conf$f1_2$f)) { 
    fit_1_2 <- get_fit(conf$f1_2, err = "Error reading fit file 2: \n")
    
    if(trim) {
      fit_1 <- get_overlapping(fit_1, fit_1_2)
      fit_2 <- get_overlapping(fit_2, fit_1_2)
      fit_1_2 <- get_overlapping(fit_1_2, fit_1)
    }
    
    f1_2_meta <- pretty_fm(get_fit_meta(conf$f1_2$f), conf$f1_2$label)
    
    f1_2_devices <- get_device_meta(conf$f1_2$f)
    
    f1_2_device_summary <- get_devices_summary(f1_2_devices)
  } else {
    fit_1_2 <- NULL
    f1_2_meta <- NULL
    f1_2_devices <- NULL
    f1_2_device_summary <- NULL
  } 
  
  return(list(f1 = list(f = fit_1, m = f1_meta, d = f1_devices, s = f1_device_summary), 
              f2 = list(f = fit_2, m = f2_meta, d = f2_devices, s = f2_device_summary),
              f1_2 = list(f = fit_1_2, m = f1_2_meta, d = f1_2_devices, s = f1_2_device_summary)))
}

#' get fit
#' @description given a fit file, return all timeseries data
#' @param x list "f" path to fit file "offset" offset in seconds
#' @param err error message
get_fit <- function(x, err = "") {
  tryCatch({
    
    fit <- read_fit_file(x$f)
    
    if(abs(x$offset) > 0) {
      fit$datetime <- as.POSIXct(fit$datetime, tz = "GMT")
      
      fit$datetime <- fit$datetime + x$offset
    }
  },
  error = function(e) {
    showNotification(paste(err, e), type = "error", duration = NULL)
    return()
  })
  
  return(fit)
}