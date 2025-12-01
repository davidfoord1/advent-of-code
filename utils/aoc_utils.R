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

  result <- suppressWarnings(
    bench::mark(eval(expr), iterations = 1)
  )

  time <- unclass(result[["total_time"]])

  if (time < 0.001) {
    time <- "< 0.001"
  } else {
    time <- round(time, 3)
  }

  mem_alloc <- unclass(result[["mem_alloc"]]) / 1024

  if (mem_alloc < 1) {
    mem_alloc <- "< 1"
  } else {
    mem_alloc <- round(mem_alloc)
  }

  cat("Elapsed:", time, "seconds\n")
  cat("Memory: ", mem_alloc, "KB\n")

  invisible(list(time, mem_alloc))
}
