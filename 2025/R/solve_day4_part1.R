solve_day4_part1 <- function(input) {
  grid <- aoc_input_to_chr_matrix(input)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)
  liftable <- matrix(FALSE, nrows, ncols)

  for (row in seq_len(nrows)) {
    for (col in seq_len(ncols)) {
      liftable[row, col] <- is_liftable(grid, row, col, nrows, ncols)
    }
  }

  sum(liftable)
}

is_liftable <- function(grid, row, col, nrows, ncols) {
  char <- grid[row, col]
  if (char != "@") {
    return(FALSE)
  }

  # count surrounding rolls
  roll_count <- 0L

  for (row_adj in -1:1) {
    search_row <- row + row_adj
    if ((search_row) < 1) next
    if ((search_row) > nrows) next

    for (col_adj in -1:1) {
      search_col <- col + col_adj
      if ((search_col) < 1) next
      if ((search_col) > ncols) next
      if (row == search_row && col == search_col) next

      if (grid[search_row, search_col] == "@") {
        roll_count <- roll_count + 1L
      }
    }
  }

  roll_count < 4
}
