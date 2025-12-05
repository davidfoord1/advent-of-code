solve_day5_part2 <- function(input) {
  split_at <- which(input == "")
  ranges <- input[1:(split_at - 1)]

  ranges <- read.table(
    text = ranges,
    sep  = "-",
    col.names = c("start", "end")
  )

  ranges <- sort_by(ranges, ranges[["start"]])

  rows_to_check <- 2:NROW(ranges)
  new_ranges <- ranges
  new_ranges[rows_to_check, ] <- NA_integer_
  prev_row <- 1L

  for (row in rows_to_check) {
    check_row <- ranges[row, ]
    stored_row <- new_ranges[prev_row, ]

    extends_prev <- check_row[["start"]] <= stored_row[["end"]] &&
                check_row["end"] >= stored_row["end"]

    if (extends_prev) {
      new_ranges[prev_row, "end"] <- ranges[row, "end"]
      next
    }

    within_prev <- check_row[["start"]] >= stored_row[["start"]] &&
      check_row[["end"]] <= stored_row[["end"]]

    if (!within_prev) {
      new_ranges[prev_row + 1L, ] <- check_row
      prev_row <- prev_row + 1L
    }
  }

  sum(new_ranges[["end"]] - new_ranges[["start"]] + 1L, na.rm = TRUE)
}
