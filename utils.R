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

get_offset <- function(offset) {
  if(offset != "") {
    
    offset <- try(as.POSIXct(offset, tz = "GMT"))
    
    if(!(inherits(offset, "POSIXct"))) {
      showNotification("Invalid datetime format. 'yyyy-mm-dd HH:MM:SS'", 
                       type = "error", duration = NULL)
      return(NULL)
    }
    
    return(offset)
    
  }
  
  return("")
}
