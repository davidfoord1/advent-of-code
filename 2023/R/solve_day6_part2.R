solve_day6_part2 <- function(input) {
  race_time <- stringi::stri_extract_all(input[[1]], regex = "\\d+")
  race_time <- as.numeric(paste0(unlist(race_time), collapse = ""))

  record <- stringi::stri_extract_all(input[[2]], regex = "\\d+")
  record <- as.numeric(paste(unlist(record), collapse = ""))

  # I originally did brute force approach over half of the numbers
  # (recognising bell shaped curve) with:
  # hold_time * (race_time - hold_time) > record

  # Solved in <10s, but even better expanding the formula:

  # -hold_time * hold_time + race_time * hold_time > record
  # -hold_time^2 + race_time * hold_time > record
  # -hold_time^2 + race_time * hold_time - record > 0

  # i.e. ax^2 + bx + c > 0

  # x = hold_time
  # a = -1
  # b = race_time
  # c = -record

  # plug into quadratic formula
  # x = (-b +- sqrt(b^2 - 4ac)) / 2a

  x1 <- (-race_time - sqrt(race_time^2 - 4 * -1 * (-record))) / (2 * -1)
  x2 <- (-race_time + sqrt(race_time^2 - 4 * -1 * (-record))) / (2 * -1)

  abs(x2 - x1)
}
