#' Find how many reports are safe with dampeners
#'
#' Check each report is
#'   either all decreasing OR all increasing
#'   AND all absolute differences are within 1 to 3
#'
#' As well as whether the same can be achieved by removing one of the levels.
#'
#' @param input
#' A character vector, where each represents  a report, containing space
#' delimited sequence of digits, 'levels'
#'
#' @return
#' numeric(1) The number of safe reports.
solve_day2_part2 <- function(input) {
  num_list <- strsplit(input, " ")
  num_list <- lapply(num_list, as.numeric)

  safe_list <- vapply(num_list, is_safe, logical(1))

  sum(safe_list)
}

#' Check if a report is safe
#'
#' Find the majority direction in differences (increasing/decreasing) if there
#' is one. Mark the minority direction as unsafe diffs. Mark absolute diffs
#' that aren't in 1, 2, 3 as unsafe. For each unsafe diff check if the report
#' could be safe with new reports made by removing the levels either side of
#' it.
#'
#' If there is no majority direction then the report is unsafe because the
#' dampener won't be enough. (I suppose this assumes report length is not 3,
#' which could be made safe)
#'
#' @param report
#' Numeric levels to be checked
#'
#' @return
#' logical(1) whether the report is safe
is_safe <- function(report, removed = 0) {
  diffs <- diff(report)
  majority_diffs <- length(diffs) / 2 + 1

  # no majority direction
  # fails all increasing OR all decreasing rule
  # dampener won't be enough
  if (max(sum(diffs < 0), sum(diffs > 0)) < majority_diffs) {
    return(FALSE)
  }

  # logical to store location of unsafe diffs
  unsafe_diffs <- logical(length(diffs))

  # if majority decreasing, unsafe where increasing
  if (sum(diffs < 0) >= 3) {
    unsafe_diffs <- diffs > 0
  }

  # if majority increasing, unsafe where decreasing
  if (sum(diffs > 0) >= 3) {
    unsafe_diffs <- unsafe_diffs | diffs < 0
  }

  # unsafe where difference not in safe size range
  unsafe_diffs <- unsafe_diffs | !abs(diffs) %in% c(1, 2, 3)

  if (!any(unsafe_diffs)) {
    # no unsafe levels
    TRUE
  } else {
    # create new reports by removing each level either side of the unsafe diffs
    unsafe_levels <- which(unsafe_diffs)
    unsafe_levels <- c(unsafe_levels, unsafe_levels + 1)
    new_reports <- lapply(unsafe_levels, \(level) report[-level])

    # check if the shortened report is safe without further option
    # to remove levels
    any(vapply(new_reports, is_safe_no_dampener, logical(1)))
  }
}

#' Check if the rest of a report is safe
#'
#' Check the differences for
#'   either all less than zero OR all greater than zero
#'   AND all absolute differences are within 1 to 3
#'
#' @param report
#' Numeric levels to be checked
#'
#' @return
#' logical(1) whether the report is safe
is_safe_no_dampener <- function(report) {
  diffs <- diff(report)

  one_direction <- all(diffs < 0) | all(diffs > 0)
  safe_size <- all(abs(diffs) %in% c(1, 2, 3))

  one_direction & safe_size
}
