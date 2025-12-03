solve_day3_part2 <- function(input) {
  input |>
    strsplit("") |>
    lapply(as.integer) |>
    vapply(max_jolt, numeric(1)) |>
    sum()
}

#' @params bank Character vector of digits
#' @params digit_count Numeric number of digits to use for joltage
max_jolt <- function(bank, digit_count = 12) {
  digit_nums <- seq_len(digit_count)
  digits <- character(digit_count)
  prev_loc <- 0L
  len <- length(bank)

  for (i in digit_nums) {
    max_loc <- len - (digit_count - i)
    valid_bank <- bank[(prev_loc + 1L):max_loc]
    loc <- which.max(valid_bank)
    prev_loc <- prev_loc + loc
    digits[i] <- valid_bank[loc]
  }

  joltage <- paste0(digits, collapse = "")

  as.numeric(joltage)
}
