#' Find the number of unique anti node locations
#'
#' Iterate over the grid creating a lookup table of antenna locations against
#' their frequency (ie represented by the same character). For each frequency
#' find the antinodes they produce. Count the unique antinode locations across
#' all frequencies.
#'
#' Bounds checking is done in `find_antinodes()`
#'
#' @param input
#' Character vector of rows in the antennas map
#'
#' @return
#' numeric(1) The number of many unique antinode locations
solve_day8_part2 <- function(input) {
  grid <- matrix(unlist(strsplit(input, "")), nrow = length(input), byrow = TRUE)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)

  antennas <- new.env(paren = emptyenv())

  # store each position for the same letter in the hashtab
  for (row in seq_len(NROW(grid))) {
    for (col in seq_len(NCOL(grid))) {
      frequency <- grid[row, col]

      if (frequency != ".") {
        antennas[[frequency]] <- c(antennas[[frequency]], list(c(row, col)))
      }
    }
  }

  antinodes <- eapply(nodes, find_antinodes, nrows, ncols)
  antinodes <- unique(unlist(antinodes, recursive = FALSE))

  length(antinodes)
}

#' Find all antinode locations for one frequency
#'
#' For each antenna before the last: find the distance between it and the
#' following antennas in the list in pairs. Move any multiple of that distance
#' away from each node in the pair and you have the location of an antinode, as
#' well as on the antenna positions themselves.
#'
#' Taking the distance as second position - first position, subtract the
#' distance from the first position and add it to the second.
#'
#' So for antennas at (3, 3) and (5, 5).
#' Distance = (5, 5) - (3, 3) = (2, 2)
#'
#' Antinodes are:
#' (3, 3) - (2, 2) = (1, 1) - next position in this direction
#'
#' (5, 5) + (2, 2), = (7, 7)
#' (7, 7) + (2, 2) = (9, 9)
#' (9, 9) + (2, 2) = (11, 11)
#' ... until out of bounds
#'
#' @param antennas
#' A list of numeric(2), every position of an antenna of one frequency
#'
#' @return
#' A list of numeric(2), every position of antinodes for the given `antennas`
find_antinodes <- function(antennas, nrows, ncols) {
  antinodes <- vector("list")

  # iterate first in pair
  for (i in 1:(length(antennas) - 1)) {
    # iterate second in pair
    for (j in (i+1):length(antennas)) {
      pos1 <- antennas[[i]]
      pos2 <- antennas[[j]]

      dist <- pos2 - pos1

      # moving away from the first antenna
      while (!out_of_bounds(pos1, nrows, ncols)) {
        antinodes <- c(antinodes, list(pos1))
        pos1 <- pos1 - dist
      }

      # moving away from the second antenna
      while (!out_of_bounds(pos2, nrows, ncols)) {
        antinodes <- c(antinodes, list(pos2))
        pos2 <- pos2 + dist
      }
    }
  }

  antinodes
}

#' Check whether a position out of grid boundaries
#'
#' Given a position (row, col), return TRUE when either are less than 1 and
#' greater than their respective limits.
#'
#' @param pos
#' numeric(2) or list(numeric(2)) to check is within bounds
#' @param nrows
#' numeric(1) row limit
#' @param ncols
#' numeric(1) col limit
#'
#' @return
#' logical(1) Whether the position is out of bounds
out_of_bounds <- function(pos, nrows, ncols) {
  pos <- unlist(pos)

  pos[[1]] <= 0 || pos[[1]] > nrows ||
    pos[[2]] <= 0 || pos[[2]] > ncols
}