solve_day2_part2 <- function(input) {
   colours <- c("r", "g", "b")

   minimums <- lapply(colours, minimum_cubes, games = input)
   powers   <- Reduce(`*`, minimums)

   sum(powers)
}

minimum_cubes <- function(colour, strings, games) {
  # all drawn for the colour per game
  pattern <- paste0("\\d+(?= ", colour, ")")
  values  <- stringi::stri_extract_all(games, regex = pattern)

  # highest drawn per game
  vapply(values, \(x) max(as.numeric(x)), numeric(1))
}
