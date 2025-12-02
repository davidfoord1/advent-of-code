solve_day2_part1 <- function(input) {
  # back to one line
  ranges <- paste0(input, collapse = "")
  ranges <- strsplit(ranges, ",", fixed = TRUE) |>
    unlist()

  ranges <- strsplit(ranges, "-", fixed = TRUE)

  invalid_total <- 0L

  for (range in ranges) {
    ids <- range[[1]]:range[[2]]

    is_invalid <- vapply(ids, is_invalid, logical(1))

    invalid_total <- invalid_total + sum(ids[is_invalid])
  }

  invalid_total
}

is_invalid <- function(id) {
  nchars <- nchar(id)

  if (nchars %% 2 == 1) {
    return(FALSE)
  }

  split <- nchars %/% 2

  substr(id, 1, split) == substr(id, split + 1, nchars)
}
