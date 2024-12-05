#' Complete an X-MAS word search
#'
#' Iterate over the word search as a matrix. When you find an "A", check
#' the diagonals crossing it for 2 "MAS"s.
#'
#' @param input
#' A character vector of lines as part of a word search grid
#'
#' @return
#' numeric(1) Occurrences of crossing "MAS"s in the word search
solve_day4_part2 <- function(input) {
  # get input into a matrix
  # this clunky line is because I've stuck to readLines for the input
  grid <- t(matrix(unlist(strsplit(input, "")), nrow = length(input)))

  # add padding to avoid out of bounds checks
  empty_row <- rep(".", NCOL(grid))
  grid <- rbind(empty_row, grid, empty_row)

  empty_col <- rep(".", NROW(grid))
  grid <- cbind(empty_col, grid, empty_col)

  x_mas_count <- 0

  # go through each position in the grid
  for (row in seq_len(NROW(grid))) {
    for (col in seq_len(NCOL(grid))) {

      # counting the number of "MAS"s in a cross formation around an "A"
      if (grid[row, col] == "A") {
        x_mas_count <- x_mas_count + find_x_mas(grid, row, col)
      }

    }
  }

  x_mas_count
}

#' Search diagonals from an "A" for "M" and "S"
#'
#' In 2 diagonals, check the adjacent positions contain "M" and "S". Sort
#' each diagonal so it doesn't matter which way around the "M" and "S" are.
#'
#' @param grid
#' Character matrix to search.
#' @param row
#' Row position of "A" to search from.
#' @param col
#' Column position of "A" to search from.
#'
#' @return
#' numeric(1) 1 when a cross of "MAS" was found at the given location,
#' otherwise 0.
find_x_mas <- function(grid, row, col) {
  words <- list()
  # up left  down right
  words[[1]] <- sort(c(grid[row - 1, col - 1], grid[row + 1, col + 1]))
  # up-right down-left
  words[[2]] <- sort(c(grid[row - 1, col + 1], grid[row + 1, col - 1]))

  sum(vapply(words, \(word) identical(word, c("M", "S")), logical(1))) == 2
}

