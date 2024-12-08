solve_day6_part2 <- function(input) {
  grid <- t(matrix(unlist(strsplit(input, "")), nrow = length(input)))

  pos <- which(grid == "^", arr.ind = TRUE)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)

  # directions in sequence of right-hand 90 degree turns
  # UP RIGHT DON LEFT
  dirs <- list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1))
  dir_num <- 1

  visited <- matrix(FALSE, nrow = nrows, ncol = ncols)
  obstr_count <- 0

  repeat {
    # store direction for loop detection
    if (visited[pos] == 0) {
      visited[pos] <- dir_num
    }

    next_pos <- pos + dirs[[dir_num]]
    # stop when out of bounds
    if (next_pos[[1]] > NROW(grid) || next_pos[[2]] > NCOL(grid)
        || next_pos[[1]] <= 0 || next_pos[[2]] == 0) {
      break
    }

    # turn when meet an obstacle
    if (grid[next_pos] == "#") {
      # cycle dirs
      dir_num <- next_dir_num(dirs, dir_num)
    } else {
      # try obstacle when haven't met an obstacle
      if (visited[next_pos] == 0) {
        obstr_count <- obstr_count + has_loop(grid, pos,
                                              dirs, dir_num,
                                              nrows, ncols)
      }

      pos <- next_pos
    }
  }

  obstr_count
}

has_loop <- function(grid, pos, dirs, dir_num, nrow_grid, ncol_grid) {
  grid[pos + dirs[[dir_num]]] <- "#"
  dir_num <- (dir_num %% length(dirs)) + 1  # Turn 90 degrees

  # 3D array for visited positions and directions
  been <- array(FALSE, dim = c(nrow_grid, ncol_grid, length(dirs)))

  repeat {
    if (been[pos[[1]], pos[[2]], dir_num]) {
      return(TRUE)  # Loop detected
    }
    been[pos[[1]], pos[[2]], dir_num] <- TRUE

    next_pos <- pos + dirs[[dir_num]]
    if (next_pos[[1]] > nrow_grid || next_pos[[2]] > ncol_grid ||
        next_pos[[1]] <= 0 || next_pos[[2]] <= 0) {
      break
    }

    if (grid[next_pos] == "#") {
      dir_num <- (dir_num %% length(dirs)) + 1  # Turn
    } else {
      pos <- next_pos
    }
  }

  FALSE
}
