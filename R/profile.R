#' get max average powers from fit power time series
#' @description Creates a "critical power" curve from a timeseries of power data.
#' @param fit data.frame containing a "datetime" and "power" column
#' @param wthn second data.frame that overlaps the first.
#' @export
#' @examples
#' get_maxes(read_fit_file(system.file("fit/fit1.fit", package = "dualR")))
#' get_maxes(read_fit_file(system.file("fit/fit1.fit", package = "dualR")),
#'           read_fit_file(system.file("fit/fit2.fit", package = "dualR")))
get_maxes <- function(fit, wthn = NULL) {

  if(!is.null(wthn)) {
    fit <- get_overlapping(fit, wthn)
  }

  fit <- to_1hz(fit, "power")

  l <- utils::tail(fit$datetime, n = 1) - fit$datetime[1]

  w <- c(1, 5, 10, 15, 30,
         60, 60 * 2, 60 * 3, 60 * 5, 60 * 10, 60 * 15, 60 * 20, 60 * 30, 60 * 45,
         60 * 60, 60 * 90, 60 * 120, 60 * 180, 60 * 240, 60 * 300)

  w <- w[w < as.numeric(l, units = "secs")]

  m <- as.numeric(lapply(w, function(window, d) {
    max(RcppRoll::roll_mean(d, window, na.rm = TRUE))
  }, d = fit$power))

  if(is.na(m[1])) m[1] <- max(fit$power, na.rm = TRUE)

  names(m) <- c("1s", "5s", "10s", "15s", "30s",
                "1m", "2m", "3m", "5m", "10m", "15m", "20m", "30m", "45m",
                "1h", "1.5h", "2h", "3h", "4h", "5h")[1:length(w)]

  m
}


get_power_summary <- function(fit) {
  fit <- to_1hz(fit, "power")
  return(list(mean = mean(fit$power, na.rm = TRUE)))
}

xPower <- function(fit_1hz) {
  mean(RcppRoll::roll_mean(fit_1hz$power^4, 25, na.rm = TRUE), na.rm = TRUE)^(1/4)
}

to_1hz <- function(fit, col = "power") {
  v <- data.frame(datetime = seq(fit$datetime[1], utils::tail(fit$datetime, n = 1), by = "1 sec"))
  v[[col]] <- stats::approx(fit$datetime, fit[[col]], v$datetime, "linear")$y
  v
}

#' get overlapping portion of timeseries
#' @inheritParams get_maxes
#' @export
get_overlapping <- function(fit, wthn) {
  
  fit <- fit[!is.na(fit$power), ]
  wthn <- wthn[!is.na(wthn$power), ]
  
  fit <- fit[fit$datetime > wthn$datetime[1] &
               fit$datetime < utils::tail(wthn$datetime, n = 1), ]
  
  if(nrow(fit) == 0) stop("provided within filter records don't overlap.")
 
  fit
}