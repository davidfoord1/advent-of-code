solve_day2_part1 <- function(input) {
  num_list <- strsplit(input, " ")
  num_list <- lapply(num_list, as.numeric)

  diff_list <- lapply(num_list, diff)

  safe_count <- 0

  for (diffs in diff_list) {
    diffs <- unlist(diffs)
    if ((all(diffs < 0) | all(diffs > 0)) & all(abs(diffs) %in% c(1, 2, 3))) {
      safe_count <- safe_count + 1
    }
  }

  safe_count
}