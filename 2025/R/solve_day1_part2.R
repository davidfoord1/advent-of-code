solve_day1_part2 <- function(input) {
  pos <- 50L
  dial_size <- 100L
  counter <- 0L

  dir <- substr(input, 1L, 1L)
  val <- as.integer(substr(input, 2L, nchar(input)))
  turns <- ifelse(dir == "R", val, -val)

  for (turn in turns) {
    # position on infinite number line
    abs_pos <- pos + turn

    # count multiple of 100 between the start and end
    if (turn > 0) {
      counter <- counter +
        (abs_pos %/% dial_size) -
        (pos %/% dial_size)
    } else {
      counter <- counter +
        (pos - 1L) %/% dial_size -
        (abs_pos - 1L) %/% dial_size
    }

    pos <- (pos + turn) %% dial_size
  }

  counter
}
