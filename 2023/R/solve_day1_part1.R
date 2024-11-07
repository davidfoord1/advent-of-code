solve_day1_part1 <- function(input) {
  first_digit <- stringi::stri_extract(input, regex = "\\d")

  reversed <- stringi::stri_reverse(input)
  last_digit <- stringi::stri_extract(reversed, regex = "\\d")

  calibration_numbers <- as.numeric(paste0(first_digit, last_digit))

  sum(calibration_numbers)
}