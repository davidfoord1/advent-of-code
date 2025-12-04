solve_day4_part2 <- function(input) {
  grid  <- aoc_input_to_chr_matrix(input)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)
  dir_list <- aoc_2D_dirs()

  surrounding <- count_surrounding(grid, dir_list, nrows, ncols)

  max_queue <- 5e4L
  remove_queue <- matrix(NA_real_, nrow = max_queue, ncol = 2L)

  first_removals <- which(surrounding < 4, arr.ind = TRUE)

  queue_start <- 1L
  queue_end   <- NROW(first_removals)
  remove_queue[queue_start:queue_end, ] <- first_removals

  total_removed <- 0L

  while (queue_start <= queue_end) {
    pos <- matrix(remove_queue[queue_start, ], nrow = 1)
    queue_start <- queue_start + 1L

    if (!is.finite(surrounding[pos])) next

    # remove roll
    total_removed <- total_removed + 1L
    surrounding[pos] <- Inf

    # update neighbours and check if they should be removed
    for (dir in dir_list) {
      change_pos <- pos + dir
      if (aoc_2D_out_of_bounds(change_pos, nrows, ncols)) next

      val <- surrounding[change_pos]
      if (!is.finite(val)) next

      surrounding[change_pos] <- val - 1

      # they should be removed if going from 4 to 3 neighbouring rolls
      if (val == 4L) {
        queue_end <- queue_end + 1L
        remove_queue[queue_end, ] <- change_pos
      }
    }
  }

  total_removed
}

#' Where previously I was checking in 8 directions from each position,
#' this version shifts the whole grid in each direction,
#' so it can do a vectorised addition to count
count_surrounding <- function(grid, dir_list, nrows, ncols) {
  surrounding <- matrix(0L, nrows, ncols)

  is_roll <- grid == "@"

  for (dir in dir_list) {
    row_adj <- dir[1L]
    col_adj <- dir[2L]

    # in bounds sequence of positions to check for rolls
    check_rows <- seq(max(1, 1 - row_adj), min(nrows, nrows - row_adj))
    check_cols <- seq(max(1, 1 - col_adj), min(ncols, ncols - col_adj))

    # positions to update count for
    dest_rows <- check_rows + row_adj
    dest_cols <- check_cols + col_adj

    surrounding[dest_rows, dest_cols] <- surrounding[dest_rows, dest_cols] +
      is_roll[check_rows, check_cols]
  }

  surrounding[!is_roll] <- Inf

  surrounding
}
