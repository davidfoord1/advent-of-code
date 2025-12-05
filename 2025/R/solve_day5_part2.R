solve_day5_part2 <- function(input) {
  split_at <- which(input == "")

  ranges <- input[1:(split_at - 1)]
  ranges <- strsplit(ranges, "-", fixed = TRUE)
  ranges <- lapply(ranges, as.numeric)
  ranges <- t(simplify2array(ranges))

  new_ranges <- matrix(nrow=NROW(ranges), ncol=NCOL(ranges))
  new_ranges <- data.frame(new_ranges)

  ranges <- data.frame(ranges)
  ranges <- sort_by(ranges, ranges[[1L]])

  new_ranges[1L, ] <- ranges[1L,]
  prev_row <- 1L

  for (row in 2:NROW(ranges)) {

    extends <- extends_prev(ranges[row, ], new_ranges[prev_row, ])
    if (extends) {
      new_ranges[prev_row, 2] <- ranges[row, 2]
    }

    within <- within_prev(ranges[row, ], new_ranges[prev_row, ])
    if (!within) {
      new_ranges[prev_row + 1L, ] <- ranges[row, ]
      prev_row <- prev_row + 1L
    }
  }

  sum(new_ranges[[2L]] - new_ranges[[1L]] + 1L, na.rm = TRUE)
}

extends_prev <- function(check_row, stored_row) {
  check_row[, 1L] <= stored_row[, 2L] &&
    check_row[, 2L] >= stored_row[, 2L]
}

within_prev <- function(check_row, stored_row) {
  check_row[, 1L] >= stored_row[, 1L] &&
    check_row[, 2L] <= stored_row[, 2L]
}
