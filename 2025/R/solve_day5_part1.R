solve_day5_part1 <- function(input) {
  split_at <- which(input == "")

  ranges <- input[1:(split_at - 1)]
  ranges <- read.table(
    text = ranges,
    sep  = "-",
    col.names = c("start", "end")
  )

  ids <- input[(split_at + 1L):length(input)]
  ids <- as.numeric(ids)

  fresh_ids <- vapply(
    ids,
    \(id) any(id >= ranges[["start"]] & id <= ranges[["end"]]),
    logical(1)
  )

  sum(fresh_ids)
}
