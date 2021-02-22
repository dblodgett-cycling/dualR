pretty_fm <- function(fm, l) {

  fm[sapply(fm, is.null)] <- ""

  sprintf("<h4>%s</h4> <br/> Manufacturer: %s <br/> Serial Number: %s <br/> Created: %s",
          l, fm$manufacturer, fm$serial_number, fm$time_created)
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

get_dygraph_data <- function(fit_1, fit_2 = NULL, l1 = NULL, l2 = NULL) {

  p <- xts(fit_1$power, order.by = fit_1$datetime)
  
  tzone(p) <- "GMT"

  dat <- list(
    power = setNames(p,
                      l1))

  dat <- add_data(fit_1, "elevation", "Elevation", l1, dat)

  dat <- add_data(fit_1, "heartrate", "Heart Rate", l1, dat)

  dat <- add_data(fit_1, "cadence", "Cadence", l1, dat)

  if(!is.null(fit_2)) {
    p <- xts(fit_2$power, order.by = fit_2$datetime)
    
    tzone(p) <- "GMT"
    
    dat$power <- cbind(dat$power,
                       setNames(p,
                                l2))

    dat <- add_data(fit_2, "elevation", "Elevation", l2, dat)

    dat <- add_data(fit_2, "heartrate", "Heart Rate", l2, dat)

    dat <- add_data(fit_2, "cadence", "Cadence", l2, dat)
  }

  return(dat)

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

powercurve_plot <- function(max_1, max_2 = NULL, l1 = NULL, l2 = NULL) {
  {

    par(las=2)

    max_1_pch <- 16

    max_2_pch <- 17

    if(is.null(max_2)) {

      plot_range <- seq(min(max_1), max(max_1), length.out = length(max_1))

      legend_text <- c(l1)
      legend_pch <- max_1_pch
      legend_color <- "black"

      point_col <- rep("blue", length(max_1))

    } else {

      plot_range <- seq(min(max_1, max_2), max(max_1, max_2), length.out = length(max_1))

      legend_text <- c(l1, l2, "diff < 2%", "2% < diff < 4%", "4% < diff < 8%", "diff > 8%")
      legend_pch <- c(max_1_pch, max_2_pch, 15, 15, 15, 15)

      perc_diff <- get_perc_diff(max_1, max_2)

      point_col <- ifelse(perc_diff < 0.02, "green",
                          ifelse(perc_diff < 0.04, "yellow",
                                 ifelse(perc_diff < 0.08, "orange", "red")))

      legend_color <- c("black", "black", "green", "yellow", "orange", "red")

    }

    plot(plot_range,
         xaxt = "n", ylab = "power (watts)", xlab = NA, col = NA)

    axis(1, at = 1:length(max_1), labels = names(max_1), cex.axis = .9)

    points(max_1, col = point_col, pch = max_1_pch, cex = 2)

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

get_powercurve_table <- function(max_1, max_2 = NULL, l1 = NULL, l2 = NULL) {
  
  if(!is.null(max_2)) {
    maxes <- list(max_1, max_2)
    
    lens <- lengths(maxes)
    
    if(!all(lens == max(lens))) {
      
      nms <- names(maxes[[which(lens == max(lens))]])
      
      max_1 <- make_full(max_1, nms)
      max_2 <- make_full(max_2, nms)
      
    }
    
    data.frame(p = names(max_1),
               p1 = as.numeric(max_1),
               p2 = as.numeric(max_2),
               d = as.numeric(get_perc_diff(max_1, max_2) * 100)) %>%
      knitr::kable(format = "html", digits = c(0, 0, 0, 1), align = "c",
                   col.names = c("Period", l1, l2, "% Diff"),
                   padding = 2) %>%
      kableExtra::kable_styling()
  } else {
    data.frame(p = names(max_1),
               p1 = as.numeric(max_1)) %>%
      knitr::kable(format = "html", digits = 0, align = "c",
                   col.names = c("Period", l1),
                   padding = 2)  %>%
      kableExtra::kable_styling()
  }
}

get_devices_table <- function(devices) {
  if(nrow(devices) == 0) devices <- data.frame(manufacturer = "undeclared", 
                                               product = "undeclared", 
                                               serial_number = "undeclared")
  
  names(devices) <- gsub("_", " ", names(devices))
  
  knitr::kable(devices, padding = 2, format = "html") %>%
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

get_conf <- function(input) {
  
  temp <- list(f = NULL, label = NULL, offset = NULL)
  
  conf <- list(f1 = temp, f1_2 = temp, f2 = temp)
  
  if(input$demo) {
    
    conf$f1$f <- "inst/fit/fit1.fit"
    conf$f1$label <- "Demo Source 1"
    conf$f1$offset <- input$f1_offset
    conf$f2$f <- "inst/fit/fit2.fit"
    conf$f2$label <- "Demo Source 2"
    conf$f2$offset <- input$f2_offset
    
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

get_dat <- function(conf, trim) {
  
  fit_1 <- get_fit(conf$f1, err = "Error reading fit file 1: \n")
  
  f1_meta <- pretty_fm(get_fit_meta(conf$f1$f), conf$f1$label)
  
  f1_devices <- get_device_meta(conf$f1$f)
  
  if(!is.null(conf$f2$label)) {
    
    fit_2 <- get_fit(conf$f2, err = "Error reading fit file 2: \n")
    
    if(trim) {
      fit_1 <- get_overlapping(fit_1, fit_2)
      fit_2 <- get_overlapping(fit_2, fit_1)
    }
    
    f2_meta <- pretty_fm(get_fit_meta(conf$f1$f), conf$f2$label)
    
    f2_devices <- get_device_meta(conf$f2$f)
    
  } else {
    
    fit_2 <- NULL
    f2_meta <- NULL
    f2_devices <- NULL
    
  }
  
  return(list(f1 = fit_1, m1 = f1_meta, d1 = f1_devices, 
              f2 = fit_2, m2 = f2_meta, d2 = f2_devices))
}

get_fit <- function(x, err) {
  tryCatch({
    
    fit_1 <- read_fit_file(x$f)
    
    if(abs(x$offset) > 0) {
      fit_1$datetime <- as.POSIXct(fit_1$datetime, tz = "GMT")
      
      fit_1$datetime <- fit_1$datetime + x$offset
    }
  },
  error = function(e) {
    showNotification(paste(err, e), type = "error", duration = NULL)
    return()
  })
  
  return(fit_1)
}