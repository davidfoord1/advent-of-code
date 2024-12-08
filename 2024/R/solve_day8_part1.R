#' Find the number of unique anti node locations
#'
#' Iterate over the grid creating a lookup table of antenna locations against
#' their frequency (ie represented by the same character). For each frequency
#' find the antinodes they produce. Take the unique antinode locations across
#' all frequencies, then count the number that are in bounds.
#'
#' @param input
#' Character vector of rows in the antennas map
#'
#' @return
#' numeric(1) The number of many unique antinode locations
solve_day8_part1 <- function(input) {
  grid <- matrix(unlist(strsplit(input, "")), nrow = length(input), byrow = TRUE)
  nrows <- NROW(grid)
  ncols <- NCOL(grid)

  # lookup ttable
  antennas <- new.env(paren = emptyenv())

  # store each position for the same letter
  for (row in seq_len(NROW(grid))) {
    for (col in seq_len(NCOL(grid))) {
      frequency <- grid[row, col]

      if (frequency != ".") {
        antennas[[frequency]] <- c(antennas[[frequency]], list(c(row, col)))
      }
    }
  }

  # iterate over frequencies finding their antinodes
  antinodes <- eapply(antennas, find_antinodes)
  antinodes <- unique(unlist(antinodes, recursive = FALSE))

  inbounds <- !vapply(antinodes, out_of_bounds, logical(1), nrows, ncols)

  sum(inbounds)
}

#' Find all antinode locations for one frequency
#'
#' For each antenna before the last: find the distance between it and the
#' following antennas in the list in pairs. Move that distance away from each
#' node in the pair and you have the location of an antinode.
#'
#' Taking the distance as second position - first position, subtract the
#' distance from the first position and add it to the second.
#'
#' So for antennas at (3, 3) and (5, 5).
#' Distance = (5, 5) - (3, 3) = (2, 2)
#' Antinodes are (3, 3) - (2, 2) = (1, 1) and (5, 5) + (2, 2), = (7, 7)
#'
#' @param antennas
#' A list of numeric(2), every position of an antenna of one frequency
#'
#' @return
#' A list of numeric(2), every position of antinodes for the given `antennas`
find_antinodes <- function(antennas) {
  antinodes <- vector("list")

  # iterate first in pair
  for (i in 1:(length(antennas) - 1)) {
    # iterate second in pair
    for (j in (i+1):length(antennas)) {
      pos1 <- antennas[[i]]
      pos2 <- antennas[[j]]

      dist <- pos2 - pos1

      new_pos1 <- pos1 - dist
      new_pos2 <- pos2 + dist

      antinodes <- c(antinodes, list(new_pos1), list(new_pos2))
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
