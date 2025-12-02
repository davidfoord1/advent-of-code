solve_day2_part2 <- function(input) {
  ranges <- paste0(input, collapse = "")
  ranges <- strsplit(ranges, ",", fixed = TRUE) |>
    unlist()

  ranges <- strsplit(ranges, "-", fixed = TRUE)

  invalid_total <- 0

  ids <- lapply(ranges, \(x) x[[1L]]:x[[2L]]) |>
    unlist()

  is_invalid <- stringi::stri_detect(ids, regex = "^(\\d+)\\1+$")

  sum(ids[is_invalid])
}
