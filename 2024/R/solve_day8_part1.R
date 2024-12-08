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

find_antinodes <- function(nodes) {
  # for each node before the last
  # find the distance between it and the following nodes
  antinodes <- vector("list")

  for (i in 1:(length(nodes) - 1)) {
    for (j in (i+1):length(nodes)) {
      pos1 <- nodes[[i]]
      pos2 <- nodes[[j]]
      dist <- pos2 - pos1
      new_pos1 <- pos1 - dist
      new_pos2 <- pos2 + dist

      antinodes <- c(antinodes, list(new_pos1), list(new_pos2))
    }
  }

  antinodes
}

out_of_bounds <- function(pos, nrows, ncols) {
  pos <- unlist(pos)
  pos[[1]] <= 0 || pos[[1]] > nrows ||
    pos[[2]] <= 0 || pos[[2]] > ncols
}
