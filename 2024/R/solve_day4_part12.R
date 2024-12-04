solve_da4_par1 <- function(input) {

  grid <- t(matrix(unlist(strsplit(input, "")), nrow = length(input)))

  # add search cushion to avoid out of bounds checks
  empty_row <- rep(".", NCOL(grid))
  grid <- rbind(empty_row, grid, empty_row)
  grid <- rbind(empty_row, grid, empty_row)
  grid <- rbind(empty_row, grid, empty_row)

  empty_col <- rep(".", NROW(grid))
  grid <- cbind(empty_col, grid, empty_col)
  grid <- cbind(empty_col, grid, empty_col)
  grid <- cbind(empty_col, grid, empty_col)


  xmas_count <- 0
  x_mas_count <- 0
  for (row in 2:(NROW(grid) - 1)) {
    for (col in 2:(NCOL(grid) - 1)) {
      if (grid[row, col] == "X") {
        xmas_count <- xmas_count + find_sequence(grid,
                                                 row,
                                                 col,
                                                 letter_sequence,
                                                 "X")


      }

      if (grid[row, col] == "A") {
        x_mas_count <- x_mas_count + find_crosses(grid, row, col)
      }
    }
  }

  xmas_count
}

find_sequence <- function(grid, row, col, letter_sequence, next_letter) {
  words <- list()

  # up left
  words[[1]] <- c(grid[row - 1, col - 1], grid[row - 2, col - 2], grid[row - 3, col - 3])
  # up
  words[[2]] <- c(grid[row - 1, col], grid[row - 2, col], grid[row - 3, col])
  # up right
  words[[3]] <- c(grid[row - 1, col + 1], grid[row - 2, col + 2], grid[row - 3, col + 3])
  # left
  words[[4]] <- c(grid[row, col - 1], grid[row, col - 2], grid[row, col - 3])
  # right
  words[[5]] <- c(grid[row, col + 1], grid[row, col + 2], grid[row, col + 3])
  # down left
  words[[6]] <- c(grid[row + 1, col - 1], grid[row + 2, col - 2], grid[row + 3, col - 3])
  # down
  words[[7]] <- c(grid[row + 1, col], grid[row + 2, col], grid[row + 3, col])
  # down right
  words[[8]] <- c(grid[row + 1, col + 1], grid[row + 2, col + 2], grid[row + 3, col + 3])

  sum(vapply(words, \(word) identical(word, c("M", "A", "S")), logical(1)))
}

find_crosses <- function(grid, row, col) {
  words <- list()
  # up left  down right
  words[[1]] <- sort(c(grid[row - 1, col - 1], grid[row + 1, col + 1]))
  # up-right down-left
  words[[2]] <- sort(c(grid[row - 1, col + 1], grid[row + 1, col - 1]))

  sum(vapply(words, \(word) identical(word, c("M", "S")), logical(1))) == 2
}

