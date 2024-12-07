solve_day6_part1 <- function(input) {
  # get input into a matrix
  # this clunky line is because I've stuck to readLines for the input
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

  sum(visited)
}