#' Find how many reports are safe
#'
#' Check each report is
#'   either all decreasing OR all increasing
#'   AND all absolute differences are within 1 to 3
#'
#' @param input
#' A character vector of strings containing space separated digits, where
#' each string is a report and each digit is a 'level'
#'
#' @return
#' numeric(1) The number of safe reports.
solve_day2_part1 <- function(input) {
  num_list <- strsplit(input, " ")
  num_list <- lapply(num_list, as.numeric)

  diff_list <- lapply(num_list, diff)

  sum(vapply(diff_list, is_safe, logical(1)))
}

#' Check if the differences are safe
#'
#' Check the differences for
#'   either all less than zero OR all greater than zero
#'   AND all absolute differences are within 1 to 3
#'
#' @param diffs
#' Numeric differences between levels in a report
#'
#' @return
#' logical(1) whether the report differences are safe
is_safe <- function(diffs) {
  one_direction <- all(diffs < 0) | all(diffs > 0)
  safe_size <- all(abs(diffs) %in% c(1, 2, 3))

  one_direction & safe_size
}
