solve_day11_part1 <- function(input, expansion_size = 2) {
  empty_rows <- which(grepl("^\\.+$", input))

  grid <- unlist(strsplit(input, ""))
  grid <- t(matrix(grid, nrow = length(input), ncol = nchar(input[[1]])))

  empty_cols <- logical(NCOL(grid))

  for(col in seq_len(NCOL(grid))) {
    empty_cols[col] <- grepl("^\\.+$", paste(grid[, col], collapse = ""))
  }

  empty_cols <- which(empty_cols)

  # find galaxy coordinates
  coords <- list()

  for (row in seq_len(NROW(grid))) {
    for (col in seq_len(NCOL(grid))) {
      if (grid[row, col] == "#") {
        coords <- c(coords, list(c(row, col)))
      }
    }
  }

  distances <- numeric()

  # find the distance between each pair of coordinates
  for (from in 1:(length(coords) - 1)) {
    for (to in (from + 1):length(coords)) {
      sorted_x <- sort(c(coords[[to]][[1]], coords[[from]][[1]]))
      sorted_y <- sort(c(coords[[to]][[2]], coords[[from]][[2]]))

      # calculate unexpanded distance
      distance <- sum(abs(sorted_x[[2]] - sorted_x[[1]]))
      distance <- distance + sum(abs(sorted_y[[2]] - sorted_y[[1]]))

      # add expansion distance for every empty row and column in between
      # the galaxies
      expand_by = expansion_size - 1

      rows_to_expand <- sum(empty_rows > sorted_x[[1]] &
                              empty_rows < sorted_x[[2]])

      cols_to_expand <- sum(empty_cols > sorted_y[[1]] &
                              empty_cols < sorted_y[[2]])

      distance <- distance + expand_by * rows_to_expand
      distance <- distance + expand_by * cols_to_expand

      distances <- c(distances, distance)
    }
  }

  sum(distances)
}
