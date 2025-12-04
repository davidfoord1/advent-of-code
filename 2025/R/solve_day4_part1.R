solve_day4_part1 <- function(input) {
  grid <- aoc_input_to_chr_matrix(input)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)
  liftable <- matrix(FALSE, nrows, ncols)

  for (row in seq_len(nrows)) {
    for (col in seq_len(ncols)) {
      liftable[row, col] <- is_liftable_2(grid, row, col, nrows, ncols)
    }
  }

  sum(liftable)
}

is_liftable <- function(grid, row, col, nrows, ncols) {
  char <- grid[row, col]
  pos <- matrix(c(row, col), nrow = 1)

  if (char != "@") {
    return(FALSE)
  }

  dir_list <- aoc_2D_dirs()
  roll_count <- 0L

  for (dir in dir_list) {
    search_pos <- pos + dir
    if (aoc_2D_out_of_bounds(search_pos, nrows, ncols)) next

    if (grid[search_pos] == "@") {
      roll_count <- roll_count + 1L
    }
  }

  roll_count < 4L
}
