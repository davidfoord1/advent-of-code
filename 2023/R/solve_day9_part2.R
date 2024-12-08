solve_day9_part2 <- function(input) {
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

  first_diffs <- rev(lapply(diffs, \(x) x[1]))
  new_diffs <- rep(0, length(first_diffs))

  for (i in seq_along(first_diffs[-length(first_diffs)])) {
    new_diffs[[i + 1]] <- first_diffs[[i + 1]] - new_diffs[[i]]
  }

  sequence[[1]] - new_diffs[[length(new_diffs)]]
}
