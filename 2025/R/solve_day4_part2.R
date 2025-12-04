solve_day4_part2 <- function(input) {
  grid <- aoc_input_to_chr_matrix(input)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)
  surrounding <- matrix(Inf, nrows, ncols)
  dir_list <- aoc_2D_dirs()
  total_removed <- 0L

  for (row in seq_len(nrows)) {
    for (col in seq_len(ncols)) {
      surrounding[row, col] <- count_surrounding(grid, row, col, nrows, ncols, dir_list)
    }
  }

  removable_count <- sum(surrounding < 4)

  while(removable_count > 0) {
    # remove rolls

    rolls_to_remove <- which(surrounding < 4, arr.ind = TRUE)
    for (i in seq_len(NROW(rolls_to_remove))) {
      pos <- rolls_to_remove[i, ]
      surrounding <- remove_roll(surrounding, pos, nrows, ncols, dir_list)
    }

    total_removed <- total_removed + removable_count
    removable_count <- sum(surrounding < 4)
  }

  total_removed
}

count_surrounding <- function(grid, row, col, nrows, ncols, dir_list) {
  pos <- matrix(c(row, col), nrow = 1)
  char <- grid[pos]

  if (char != "@") {
    return(Inf)
  }

  roll_count <- 0

  for (dir in dir_list) {
    search_pos <- pos + dir
    if (aoc_2D_out_of_bounds(search_pos, nrows, ncols)) next

    if (grid[search_pos] == "@") {
      roll_count <- roll_count + 1L
    }
  }

  roll_count
}

remove_roll <- function(surrounding, pos, nrows, ncols, dir_list) {
  pos <- matrix(pos, nrow = 1)
  surrounding[pos] <- Inf

  for (dir in dir_list) {
    change_pos <- pos + dir
    if (aoc_2D_out_of_bounds(change_pos, nrows, ncols)) next

    surrounding[change_pos] <- surrounding[change_pos] - 1
  }

  surrounding
}
