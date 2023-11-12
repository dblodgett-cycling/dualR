#' work summary
#' @description
#' get summary of kj generated from a power time series
#' @inheritParams get_maxes
#' @param bins numeric sequence defining bin edges
#' @return data.frame with bin (factor) and work (numeric) columns
#' @importFrom dplyr group_by summarise
#' @export
#' @examples
#' get_work_summary(read_fit_file(system.file("fit/fit1.fit", package = "dualR")))
#' get_work_summary(read_fit_file(system.file("fit/fit1.fit", package = "dualR")),
#'                  read_fit_file(system.file("fit/fit2.fit", package = "dualR")))
get_work_summary <- function(fit, wthn = NULL, 
                             bins = c(seq(0, 1000, 50), 5000),
                             labels = bins[1:length(bins) - 1]) {
  
  fit <- prep_fit(fit, wthn)
  
  fit$bin <- cut(fit$power, bins, labels = labels)
  
  group_by(fit, .data$bin) |>
    summarise(work = sum(.data$power) / 1000) |> 
    tidyr::complete(.data$bin) |>
    filter(!is.na(.data$bin))
  
  }
