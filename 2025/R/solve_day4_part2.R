solve_day4_part2 <- function(input) {
  grid <- aoc_input_to_chr_matrix(input)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)
  surrounding <- matrix(Inf, nrows, ncols)
  liftable <- matrix(FALSE, nrows, ncols)
  total_removed <- 0L

  for (row in seq_len(nrows)) {
    for (col in seq_len(ncols)) {
      surrounding[row, col] <- count_surrounding(grid, row, col, nrows, ncols)
    }
  }

  removable_count <- sum(surrounding < 4L)

  while(removable_count > 0) {
    # remove rolls

    rolls_to_remove <- which(surrounding < 4, arr.ind = TRUE)
    for (i in seq_len(NROW(rolls_to_remove))) {
      pos <- rolls_to_remove[i, ]
      surrounding <- remove_roll(surrounding, pos, nrows, ncols)
    }

    total_removed <- total_removed + removable_count
    removable_count <- sum(surrounding < 4L)
  }

  total_removed
}

count_surrounding <- function(grid, row, col, nrows, ncols) {
  char <- grid[row, col]
  if (char != "@") {
    return(Inf)
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

  roll_count
}

remove_roll <- function(surrounding, pos, nrows, ncols) {
  row <- pos[[1]]
  col <- pos[[2]]
  surrounding[row, col] <- Inf

  for (row_adj in -1:1) {
    change_row <- row + row_adj
    if ((change_row) < 1) next
    if ((change_row) > nrows) next

    for (col_adj in -1:1) {
      change_col <- col + col_adj
      if ((change_col) < 1) next
      if ((change_col) > ncols) next
      if (row == change_row && col == change_col) next

      surrounding[change_row, change_col] <- surrounding[change_row, change_col] - 1
    }
  }

  surrounding
}
