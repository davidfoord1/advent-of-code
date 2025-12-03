solve_day3_part2 <- function(input) {
  input |>
    strsplit("") |>
    lapply(as.integer) |>
    vapply(max_jolt_12, numeric(1)) |>
    sum()
}

max_jolt_12 <- function(bank) {
  digit_nums <- 1:12
  digits <- character(12)
  prev_loc <- 0L
  len <- length(bank)

  for (i in digit_nums) {
    max_loc <- len - (12 - i)
    valid_bank <- bank[(prev_loc + 1):max_loc]
    loc <- which.max(valid_bank)
    prev_loc <- prev_loc + loc
    digits[i] <- valid_bank[loc]
  }

  joltage <- paste0(digits, collapse = "")

  as.numeric(joltage)
}
