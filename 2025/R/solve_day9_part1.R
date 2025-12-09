solve_day9_part1 <- function(input) {
  coords <- read.table(text = input, sep = ",", col.names = c("row", "col"))
  coords <- as.matrix(coords)

  n_corners <- NROW(coords)
  pairs <- combn(n_corners, 2L)

  row_dist <- abs(coords[pairs[1, ], "row"] - coords[pairs[2, ], "row"]) + 1
  col_dist <- abs(coords[pairs[1, ], "col"] - coords[pairs[2, ], "col"]) + 1

  area <- row_dist * col_dist
  max(area)
}
