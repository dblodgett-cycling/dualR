pretty_fm <- function(fm, l) {

  fm[sapply(fm, is.null)] <- ""

  sprintf("<h2>%s</h2> <br/> Manufacturer: %s <br/> Serial Number: %s <br/> Created: %s",
          l, fm$manufacturer, fm$serial_number, fm$time_created)
}

get_dygraph_data <- function(fit_1, fit_2 = NULL, l1 = NULL, l2 = NULL) {


  dat <- list(
    power = setNames(xts(fit_1$power, order.by = fit_1$datetime),
                      l1))

  dat <- add_data(fit_1, "elevation", "Elevation", l1, dat)

  dat <- add_data(fit_1, "heartrate", "Heart Rate", l1, dat)

  dat <- add_data(fit_1, "cadence", "Cadence", l1, dat)

  if(!is.null(fit_2)) {
    dat$power <- cbind(dat$power,
                       setNames(xts(fit_2$power, order.by = fit_2$datetime),
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

get_powercurve_table <- function(max_1, max_2 = NULL, l1 = NULL, l2 = NULL, format = "html") {
  if(!is.null(max_2)) {
    data.frame(p = names(max_1),
               p1 = as.numeric(max_1),
               p2 = as.numeric(max_2),
               d = as.numeric(get_perc_diff(max_1, max_2) * 100)) %>%
      knitr::kable(format, digits = 0, align = "c",
                   col.names = c("Period", l1, l2, "% Diff"),
                   padding = 2)
  } else {
    data.frame(p = names(max_1),
               p1 = as.numeric(max_1)) %>%
      knitr::kable(format, digits = 0, align = "c",
                   col.names = c("Period", l1),
                   padding = 2)
  }
}

get_dygraph <- function(dat) {
  dygraph(dat, group = "one") %>%
    dyRangeSelector()
}

get_perc_diff <- function(max_1, max_2) {
  round((abs(max_1 - max_2) / ((max_1 + max_2) / 2)), 2)
}
