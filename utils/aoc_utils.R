aoc_read <- function(day) {
  path <- paste0("input/", day, ".txt")
  readLines(path)
}

aoc_source <- function(day, part) {
  path <- paste0("R/solve_day", day, "_part", part, ".R")
  source(path)
}
