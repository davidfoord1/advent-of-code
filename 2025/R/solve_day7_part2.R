solve_day7_part2 <- function(input) {
  grid <- aoc_input_to_chr_matrix(input)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)

  source <- which(grid == "S", arr.ind = TRUE)

  row <- source[[1L]]
  beams <- matrix(0, nrows, ncols)
  beams[row, source[[2L]]] <- 1

  for (row in seq_len(nrows-1L)) {
    curr <- beams[row, ]
    cols <- which(curr > 0)

    next_row <- row + 1L
    is_split <- grid[next_row, cols] == "^"

    splits  <- cols[is_split]
    not_split <- cols[!is_split]

    if (length(not_split) > 0L) {
      beams[next_row, not_split] <- beams[next_row, not_split] + curr[not_split]
    }

    if (length(splits) > 0L) {
      left  <- splits - 1L
      right <- splits + 1L

      beams[next_row, left]  <- beams[next_row, left]  + curr[splits]
      beams[next_row, right] <- beams[next_row, right] + curr[splits]
    }
  }

  sum(beams[nrows, ])
}
