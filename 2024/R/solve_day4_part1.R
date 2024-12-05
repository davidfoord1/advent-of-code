#' Complete an XMAS word search
#'
#' Iterate over the word search as a matrix. When you find an "X", check
#' around it for the full word "XMAS",
#'
#' @param input
#' A character vector of lines as part of a word search grid
#'
#' @return
#' numeric(1) Occurrences of "XMAS" in the word search
solve_day4_part1 <- function(input) {
  # get input into a matrix
  # this clunky line is because I've stuck to readLines for the input
  grid <- t(matrix(unlist(strsplit(input, "")), nrow = length(input)))

  # add padding to avoid out of bounds checks
  empty_row <- rep(".", NCOL(grid))
  for (i in 1:3) grid <- rbind(empty_row, grid, empty_row)

  empty_col <- rep(".", NROW(grid))
  for (i in 1:3) grid <- cbind(empty_col, grid, empty_col)

  xmas_count <- 0

  # go through each positino in the grid
  for (row in seq_len(NROW(grid))) {
    for (col in seq_len(NCOL(grid))) {

      # counting the number of "XMAS"s found from each "X"
      if (grid[row, col] == "X") {
        xmas_count <- xmas_count + find_xmas(grid, row, col)
      }

    }
  }

  xmas_count
}

#' Search lines from an "X" for "MAS"
#'
#' In 8 directions, check the next 3 grid cells for the sequence "M" "A" "S".
#'
#' @param grid
#' Character matrix to search
#' @param row
#' Row position of "X" to search from.
#' @param col
#' Column position of "X" to search from.
#'
#' @return
#' numeric(1) The number of "XMAS"s found at this location.
find_xmas <- function(grid, row, col) {
  words <- list()

  # up-left
  words[[1]] <- c(grid[row - 1, col - 1],
                  grid[row - 2, col - 2],
                  grid[row - 3, col - 3])
  # up
  words[[2]] <- c(grid[row - 1, col],
                  grid[row - 2, col],
                  grid[row - 3, col])
  # up-right
  words[[3]] <- c(grid[row - 1, col + 1],
                  grid[row - 2, col + 2],
                  grid[row - 3, col + 3])
  # left
  words[[4]] <- c(grid[row, col - 1],
                  grid[row, col - 2],
                  grid[row, col - 3])
  # right
  words[[5]] <- c(grid[row, col + 1],
                  grid[row, col + 2],
                  grid[row, col + 3])
  # down-left
  words[[6]] <- c(grid[row + 1, col - 1],
                  grid[row + 2, col - 2],
                  grid[row + 3, col - 3])
  # down
  words[[7]] <- c(grid[row + 1, col],
                  grid[row + 2, col],
                  grid[row + 3, col])
  # down-right
  words[[8]] <- c(grid[row + 1, col + 1],
                  grid[row + 2, col + 2],
                  grid[row + 3, col + 3])

  sum(vapply(words, \(word) identical(word, c("M", "A", "S")), logical(1)))
}

