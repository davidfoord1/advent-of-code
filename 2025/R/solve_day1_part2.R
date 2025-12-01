solve_day1_part2 <- function(input) {
  pos <- 50L
  dial_size <- 100L
  zero_counter <- 0L

  dir <- substr(input, 1L, 1L)
  val <- as.integer(substr(input, 2L, nchar(input)))
  turns <- ifelse(dir == "R", val, -val)

  for (turn in turns) {
    for (k in seq_len(abs(turn))) {
      if (turn < 0) {
        dir <- -1L
      } else {
        dir <- 1
      }

      pos <- pos + dir

      if (pos == -1) {
        pos <- 99
      }
      if (pos == 100) {
        pos <- 0
      }

      zero_counter <- zero_counter + (pos == 0)
    }
  }

  zero_counter
}
