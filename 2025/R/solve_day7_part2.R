solve_day7_part2 <- function(input) {
  grid <- aoc_input_to_chr_matrix(input)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)

  source <- which(grid == "S", arr.ind = TRUE)

  row <- source[[1L]] + 1L
  beams <- matrix(0, nrows, ncols)
  beams[row, source[[2L]]] <- 1L
  split_count <- 0L

  while(row < nrows) {
    next_row <- row + 1L
    cols <- which(beams[row, ] > 0)

    for (col in cols) {
      n_beams <- beams[row, col]
      if (grid[next_row, col] == "^") {
        split_count <- split_count + n_beams
        beams[next_row, col - 1L] <- beams[next_row, col - 1L] + n_beams
        beams[next_row, col + 1L] <- beams[next_row, col + 1L] + n_beams
        next
      }

      beams[next_row, col] <- beams[next_row, col] + n_beams
    }

    row <- next_row
  }

  sum(beams[nrows, ])
}
