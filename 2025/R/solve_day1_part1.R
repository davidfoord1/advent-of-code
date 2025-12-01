solve_day1_part1 <- function(input) {
  pos <- 50L
  dial_size <- 100L
  zero_counter <- 0L

  dir <- substr(input, 1L, 1L)
  val <- as.integer(substr(input, 2L, nchar(input)))

  turns <- ifelse(dir == "R", val, -val)

  for (turn in turns) {
    pos <- (pos + turn) %% dial_size

    zero_counter <- (pos == 0L) + zero_counter
  }

  zero_counter
}
