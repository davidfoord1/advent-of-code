solve_day5_part1 <- function(input) {
  split_at <- which(input == "")

  ranges <- input[1:(split_at - 1)]
  ranges <- strsplit(ranges, "-", fixed = TRUE)
  ranges <- lapply(ranges, as.numeric)
  ids <- input[(split_at + 1L):length(input)]
  ids <- as.numeric(ids)

  fresh_ids <- vapply(ids, \(id) id_is_fresh(id, ranges), logical(1))

  sum(fresh_ids)
}

id_is_fresh <- function(id, ranges) {
  for (range in ranges) {
    if (id >= range[[1]] && id <= range[[2]]) {
      return(TRUE)
    }
  }

  return(FALSE)
}
