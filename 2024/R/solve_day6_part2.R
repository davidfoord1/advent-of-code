solve_day6_part2 <- function(input) {
  grid <- t(matrix(unlist(strsplit(input, "")), nrow = length(input)))

  pos <- which(grid == "^", arr.ind = TRUE)

  # directions in sequence of right-hand 90 degree turns
  # UP RIGHT DOWN LEFT
  dirs <- list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1))
  dir_num <- 1

  visited <- matrix(FALSE, nrow = NROW(grid), ncol = NCOL(grid))

  repeat {
    if (visited[pos] == FALSE) {
      visited[pos] <- TRUE
    }

    next_pos <- pos + dirs[[dir_num]]
    # stop when out of bounds
    if (next_pos[[1]] > NROW(grid) | next_pos[[2]] > NCOL(grid)) {
      break
    }

    if (grid[next_pos] == "#") {
      # cycle dirs
      dir_num <- dir_num + 1

      if (dir_num > length(dirs)) {
        dir_num <-  1
      }
    } else {
      pos <- next_pos
    }
  }

  visited <- which(visited)
  # don't try the start position
  visited <- visited[visited != which(grid == "^")]

  has_loop <- logical(length(visited))

  for (i in seq_along(visited)) {
    has_loop[[i]] <- try_loop(visited[[i]], grid, dirs)
    print(sum(has_loop))
  }

  grid[visited[has_loop]] <- "O"

  sum(has_loop)
}

try_loop <- function(obs_pos, grid, dirs, dir_num = 1) {
  grid[obs_pos] <- "#"
  nrows <- NROW(grid)
  ncols <- NCOL(grid)

  start_pos <- which(grid == "^", arr.ind = TRUE)
  pos <- start_pos

  visited <- new.env()

  repeat {
    pos_name <- paste0(pos, collapse = ",")

    if (dir_num %in% visited[[pos_name]]) {
      return(TRUE)
    }

    visited[[pos_name]] <- c(visited[[pos_name]], dir_num)

    next_pos <- pos + dirs[[dir_num]]

    if (out_of_bounds(next_pos, nrows, ncols)) {
      break
    }

    while(grid[next_pos] == "#") {
      dir_num <- next_dir_num(dirs, dir_num)
      # visited[[pos_name]] <- c(visited[[pos_name]], dir_num)
      next_pos <- pos + dirs[[dir_num]]
    }

    # if (try_loop(grid, pos, next_pos, dir)) {
    #   obstr_count <- obstr_count + 1
    #   print(obstr_count)
    # }

    pos <- next_pos
  }

  FALSE
}


out_of_bounds <- function(pos, nrows, ncols) {
  pos[[1]] <= 0 || pos[[1]] > nrows ||
    pos[[2]] <= 0 || pos[[2]] > ncols
}