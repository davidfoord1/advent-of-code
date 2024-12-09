#' Find the number of unique locations in a guard's path
#'
#' Find the start position "^". Traverse from there until leaving the grid.
#'
#' If there is an obstacle (#) at the next location in your current direction,
#' cycle to the next direction, otherwise take a step in your current direction.
#'
#' @param input
#' Character vector of rows in laboratory grid.
#'
#' @return
#' numeric(1) Number of unique positions traversed.
solve_day6_part1 <- function(input) {
  grid <- t(matrix(unlist(strsplit(input, "")), nrow = length(input)))

  # find the start
  pos <- which(grid == "^", arr.ind = TRUE)

  # directions in sequence of right-hand 90 degree turns
  # UP RIGHT DOWN LEFT
  dirs <- list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1))
  dir_num <- 1 # start withUP

  # store positions when visited
  visited <- matrix(FALSE, nrow = NROW(grid), ncol = NCOL(grid))

  repeat {
    if (visited[pos] == FALSE) {
      visited[pos] <- TRUE
    }

    # find the next position in the current direction
    next_pos <- pos + dirs[[dir_num]]

    # stop when at the edge of the grid
    # ie the next position is out of bounds
    if (next_pos[[1]] > NROW(grid) | next_pos[[2]] > NCOL(grid)) {
      break
    }

    # cycle directions when in front of an obstacle
    if (grid[next_pos] == "#") {
      dir_num <- dir_num + 1

      if (dir_num > length(dirs)) {
        dir_num <-  1
      }
    } else {

      # step when not in front of an obstacle
      pos <- next_pos
    }
  }

  sum(visited)
}
