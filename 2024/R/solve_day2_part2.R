solve_day2_part2 <- function(input) {
  num_list <- strsplit(input, " ")
  num_list <- lapply(num_list, as.numeric)

  safe_list <- vapply(num_list, is_safe, logical(1))

  sum(safe_list)
}

is_safe <- function(report, removed = 0) {
  diffs <- diff(report)

  if (removed == 1) {
    one_direction <- all(diffs < 0) | all(diffs > 0)
    safe_amount <- all(abs(diffs) <= 3)
    return(one_direction & safe_amount)
  }

  if (max(sum(diffs < 0), sum(diffs > 0)) < 3) {
    return(FALSE)
  }

  unsafe_diffs <- logical(length(diffs))

  if (sum(diffs < 0) >= 3) {
    unsafe_diffs <- diffs >= 0
  }

  if (sum(diffs > 0) >= 3) {
    unsafe_diffs <- unsafe_diffs | diffs <= 0
  }

  unsafe_diffs <- unsafe_diffs | diffs == 0
  unsafe_diffs <- unsafe_diffs | !abs(diffs) <= 3

  if (!any(unsafe_diffs)) {
    # no unsafe levels
    TRUE
  } else {
    # try removing each level either side of the unsafe diff
    unsafe_levels <- which(unsafe_diffs)
    unsafe_levels <- c(unsafe_levels, unsafe_levels + 1)
    new_reports <- lapply(unsafe_levels, \(level) report[-level])

    any(vapply(new_reports, is_safe, logical(1), removed = 1))
  }
}
