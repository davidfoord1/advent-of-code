solve_day1_part2 <- function(input) {
  word_digits <- c("one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5,
                   "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9)


  # left to right
  first_digit <- find_first_digit(input, word_digits)

  # right to left
  input              <- stringi::stri_reverse(input)
  names(word_digits) <- stringi::stri_reverse(names(word_digits))
  last_digit         <- find_first_digit(input, word_digits)

  calibration_numbers <- as.numeric(paste0(first_digit, last_digit))

  sum(calibration_numbers)
}

find_first_digit <- function(strings, word_digits) {
  pattern <- c("\\d", names(word_digits))
  pattern <- paste0(pattern, collapse = "|")

  first_number  <- stringi::stri_extract(strings, regex = pattern)

  # convert words to digits
  is_word <- nchar(first_number) > 1

  ifelse(is_word, word_digits[first_number], first_number)
}
