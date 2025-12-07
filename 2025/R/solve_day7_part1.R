solve_day7_part1 <- function(input) {
  grid <- aoc_input_to_chr_matrix(input)

  nrows <- NROW(grid)
  ncols <- NCOL(grid)

  source <- which(grid == "S", arr.ind = TRUE)
#   splitters <- which(grid == "^", arr.ind = TRUE)

  row <- source[[1L]] + 1L
  beams <- vector("list", nrows)
  beams[[2L]] <- source[[2L]]
  split_count <- 0L

  while(row < nrows) {
    next_row <- row + 1L

    for (i in beams[[row]]) {
      if (grid[row, i] == "^") {
        split_count <- split_count + 1L
        beams[[next_row]] <- c(beams[[next_row]], i-1, i+1)
        next
      }

      if (!i %in% beams[[next_row]]) {
        beams[[next_row]] <- c(beams[[next_row]], i)
      }
    }

    row <- next_row
  }

  split_count
}
