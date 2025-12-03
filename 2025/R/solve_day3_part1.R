solve_day3_part1 <- function(input) {
  input |>
    strsplit("") |>
    lapply(as.integer) |>
    vapply(max_jolt, integer(1)) |>
    sum()
}

max_jolt <- function(bank) {
  first_loc <- which.max(bank[1:(length(bank) - 1)])
  first_digit <- bank[[first_loc]]

  second_digit <- max(bank[(first_loc + 1):length(bank)])

  joltage <- paste0(first_digit, second_digit)

  as.integer(joltage)
}
