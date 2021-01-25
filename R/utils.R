pretty_fm <- function(fm) {

  fm[sapply(fm, is.null)] <- ""

  sprintf("Manufacturer: %s <br/> Serial Number: %s <br/> Created: %s",
          fm$manufacturer, fm$serial_number, fm$time_created)
}

get_hygraph_data <- function(input, fit_1, fit_2 = NULL) {
  dat <- list(
    power = setNames(xts(fit_1$power, order.by = fit_1$datetime),
                      input$f1_label))

  dat <- add_data(fit_1, "elevation", "Elevation", input$f1_label, dat)

  dat <- add_data(fit_1, "heartrate", "Heart Rate", input$f1_label, dat)

  dat <- add_data(fit_1, "cadence", "Cadence", input$f1_label, dat)

  if(!is.null(fit_2)) {
    dat$power <- cbind(dat$power,
                       setNames(xts(fit_2$power, order.by = fit_2$datetime),
                                input$f2_label))

    dat <- add_data(fit_2, "elevation", "Elevation", input$f2_label, dat)

    dat <- add_data(fit_2, "heartrate", "Heart Rate", input$f2_label, dat)

    dat <- add_data(fit_2, "cadence", "Cadence", input$f2_label, dat)
  }

  return(dat)

}

add_data <- function(fit, field, field_name, fit_label, dat) {
  if(field %in% names(fit)) {
    out <- setNames(xts(fit[[field]], order.by = fit$datetime),
                    paste(field_name, fit_label))

    if(field %in% names(dat)) {
      dat[[field]] <- cbind(dat[[field]], out)
    } else {
      dat[[field]] <- out
    }
  }

  dat
}

powercurve_plot <- function(input, max_1, max_2 = NULL) {

  if(is.null(max_2)) {
    {
      par(las=2)
      plot(seq(min(max_1), max(max_1), length.out = length(max_1)),
           xaxt = "n", ylab = "power (watts)", xlab = NA, col = NA)
      axis(1, at = 1:length(max_1), labels = names(max_1), cex.axis = .9)

      grid(col = "grey")

      points(max_1, col = "blue", pch = 1)

      legend("topright", legend = c(input$f1_label), col = c("blue"), pch = 1)
    }
  } else {
    {
      par(las=2)
      plot(seq(min(max_1, max_2), max(max_1, max_2), length.out = length(max_1)),
           xaxt = "n", ylab = "power (watts)", xlab = NA, col = NA)
      axis(1, at = 1:length(max_1), labels = names(max_1), cex.axis = .9)

      grid(col = "grey")

      points(max_1, col = "blue", pch = 1)

      points(max_2, col = 'red', pch = 1)

      legend("topright", legend = c(input$f1_label, input$f2_label),
             col = c("blue", "red"), pch = 1)
    }
  }
}

get_powercurve_table <- function(input, max_1, max_2 = NULL) {
  if(!is.null(max_2)) {
    data.frame(p = names(max_1),
               p1 = as.numeric(max_1),
               p2 = as.numeric(max_2)) %>%
      knitr::kable("html", digits = 0, align = "c",
                   col.names = c("Period", input$f1_label, input$f2_label))
  } else {
    data.frame(p = names(max_1),
               p1 = as.numeric(max_1)) %>%
      knitr::kable("html", digits = 0, align = "c",
                   col.names = c("Period", input$f1_label))
  }
}
