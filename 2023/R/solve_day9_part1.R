solve_day9_part1 <- function(input) {
  seqs <- stringi::stri_extract_all(input, regex = "\\-*\\d+")
  seqs <- lapply(seqs, as.numeric)

  values <- lapply(seqs, extrapolate_value)

  sum(unlist(values))
}

extrapolate_value <- function(sequence) {
  diffs <- vector("list", length(sequence))
  diffs[[1]] <- diff(sequence)

  i <- 1
  while (any(diffs[[i]] != 0)) {
    next_i <- i + 1
    diffs[[next_i]] <- diff(diffs[[i]])
    i <- next_i
  }

  diffs <- diffs[!vapply(diffs, is.null, logical(1))]

  last_diff <- rev(lapply(diffs, \(x) x[length(x)]))
  new_diff <- rep(0, length(last_diff))

  for (i in seq_along(last_diff[-length(last_diff)])) {
    new_diff[[i + 1]] <- last_diff[[i + 1]] + new_diff[[i]]
  }

  sequence[[length(sequence)]] + new_diff[[length(new_diff)]]
}
