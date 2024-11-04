aoc_read <- function(day) {
  path <- paste0("input/day", day, ".txt")
  readLines(path)
}

aoc_source <- function(day, part) {
  path <- paste0("R/solve_day", day, "_part", part, ".R")
  source(path)
}

aoc_run <- function(expr) {
  expr <- substitute(expr)

  start_time <- proc.time()

  result <- eval(expr)

  time_diff <- proc.time() - start_time
  elapsed   <- round(time_diff[["elapsed"]], 2)

  if (elapsed < 0.01)
    elapsed <- "< 0.01"

  cat("Answer: ", result, "\n")
  cat("Elapsed:", elapsed, "seconds\n")

  invisible(list(result, elapsed))
}