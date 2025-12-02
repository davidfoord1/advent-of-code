solve_day2_part2 <- function(input) {
  # back to one line
  ranges <- paste0(input, collapse = "")
  ranges <- strsplit(ranges, ",", fixed = TRUE) |>
    unlist()

  ranges <- strsplit(ranges, "-", fixed = TRUE)

  invalid_total <- 0

  for (range in ranges) {
    ids <- range[[1]]:range[[2]]

    invalid_ids <- vapply(ids, is_invalid, logical(1))

    invalid_total <- invalid_total + sum(ids[invalid_ids])
  }

  invalid_total
}

is_invalid <- function(id) {
  nchars <- nchar(id)

  for (i in seq_len(nchars - 1)) {
    for (j in i:nchars) {
      substr_len <- j - i + 1

      if ((nchars - substr_len) < substr_len) {
        break
      }

      if (nchars %% substr_len != 0) {
        next
      }

      substr_chars <- substr(id, i, j)
      check_count <- (nchars - substr_len) / substr_len

      checks <- vapply(
        seq_len(check_count),
        \(multi) {
          step <- multi * substr_len
          substr(id, i + step, j + step) == substr_chars
        },
        logical(1)
      )

      if (all(checks)) return(TRUE)
    }
  }

  FALSE
}
