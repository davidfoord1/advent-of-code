solve_day6_part2 <- function(input) {
  grid <- matrix(unlist(strsplit(input, "")),
                 nrow = length(input),
                 byrow = TRUE)

  pos <- which(grid == "^", arr.ind = TRUE)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)

  # directions in sequence of right-hand 90 degree turns
  # UP RIGHT DON LEFT
  dirs <- list(c(-1L, 0L), c(0L, 1L), c(1L, 0L), c(0L, -1L))
  dir_num <- 1L
  dirs_len <- 4L

  visited <- matrix(FALSE, nrow = nrows, ncol = ncols)
  obstr_count <- 0L

  repeat {
    # store direction for loop detection
    if (visited[pos] == FALSE) {
      visited[pos] <- TRUE
    }

    next_pos <- pos + dirs[[dir_num]]
    # stop when out of bounds
    if (next_pos[[1L]] > NROW(grid) || next_pos[[2L]] > NCOL(grid)
        || next_pos[[1L]] <= 0L || next_pos[[2L]] == 0L) {
      break
    }

    # turn when meet an obstacle
    if (grid[next_pos] == "#") {
      # cycle dirs
      dir_num <- (dir_num %% dirs_len) + 1L
    } else {
      # try obstacle when haven't met an obstacle
      if (visited[next_pos] == FALSE) {
        obstr_count <- obstr_count + has_loop(grid, pos,
                                              dirs, dir_num, dirs_len,
                                              nrows, ncols)
      }

      pos <- next_pos
    }
  }

  obstr_count
}

has_loop <- function(grid, pos, dirs, dir_num, dirs_len, nrow_grid, ncol_grid) {
  grid[pos + dirs[[dir_num]]] <- "#"
  dir_num <- (dir_num %% length(dirs)) + 1L  # Turn 90 degrees

  # 3D array for visited positions and directions
  been <- array(FALSE, dim = c(nrow_grid, ncol_grid, length(dirs)))

  repeat {
    if (been[pos[[1L]], pos[[2L]], dir_num]) {
      return(TRUE)  # Loop detected
    }
    been[pos[[1L]], pos[[2L]], dir_num] <- TRUE

    next_pos <- pos + dirs[[dir_num]]
    if (next_pos[[1L]] > nrow_grid || next_pos[[2L]] > ncol_grid ||
        next_pos[[1L]] <= 0L || next_pos[[2L]] <= 0L) {
      break
    }

    if (grid[next_pos] == "#") {
      dir_num <- (dir_num %% dirs_len) + 1L  # Turn
    } else {
      pos <- next_pos
    }
  }

  FALSE
}
