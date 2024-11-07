solve_day2_part1 <- function(input) {
   colour_max <- c("r" = 12, "g" = 13, "b" = 14)

   possible <- lapply(names(colour_max),
                      is_possible,
                      games = input,
                      max = colour_max)

   possible <- Reduce(`&`, possible)

   ids <- seq_along(input)
   sum(ids[possible])
}

is_possible <- function(colour, strings, games, max) {
  # all drawn for the colour per game
  pattern <- paste0("\\d+(?= ", colour, ")")
  values  <- stringi::stri_extract_all(games, regex = pattern)

  # highest drawn per game
  highest_values <- vapply(values, \(x) max(as.numeric(x)), numeric(1))

  # highest is possible per game
  vapply(highest_values, \(x) x <= max[colour], logical(1))
}