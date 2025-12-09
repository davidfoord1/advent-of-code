solve_day9_part2 <- function(input) {
  coords <- read.table(text = input, sep = ",", col.names = c("row", "col"))
  coords <- as.matrix(coords)

  # Rectangle pairs ----
  n_corners <- NROW(coords)
  pairs <- t(combn(n_corners, 2L))
  n_pairs <- NROW(pairs)

  row_dist <- abs(coords[pairs[, 1], "row"] - coords[pairs[, 2], "row"]) + 1
  col_dist <- abs(coords[pairs[, 1], "col"] - coords[pairs[, 2], "col"]) + 1
  area <- row_dist * col_dist

  area_order <- order(area, decreasing = TRUE)
  area <- area[area_order]
  pairs <- pairs[area_order, ]

  # Rectangles bounds ----
  row_1 <- coords[pairs[, 1], "row"]
  col_1 <- coords[pairs[, 1], "col"]
  row_2 <- coords[pairs[, 2], "row"]
  col_2 <- coords[pairs[, 2], "col"]

  rects <- data.frame(
    min_row = pmin(row_1, row_2),
    max_row = pmax(row_1, row_2),
    min_col = pmin(col_1, col_2),
    max_col = pmax(col_1, col_2)
  )

  # Intersection check ----

  # Check each rect against each polygon edge for intersection
  # not a true polygon intersection but works
  # based on visual look at shape with plot(coords)
  for (rect_i in seq_len(n_pairs)) {
    rect <- rects[rect_i, ]
    rect_is_valid <- TRUE

    for (edge_i in seq_len(n_corners)) {
      point_1 <- coords[edge_i, ]

      if (edge_i == n_corners) {
        # last edge back to start
        edge_j <- 1
      } else {
        edge_j <- edge_i + 1
      }

      point_2 <- coords[edge_j, ]

      intersects <- edge_intersects_rect(rect, point_1, point_2)

      if (intersects) {
        rect_is_valid <- FALSE
        break
      }
    }

    if (rect_is_valid) {
      return(area[rect_i])
    }
  }
}

edge_intersects_rect <- function(rect, point_1, point_2) {
  row_1 <- point_1["row"]
  col_1 <- point_1["col"]
  row_2 <- point_2["row"]
  col_2 <- point_2["col"]

  # Horizontal edge
  if (row_1 == row_2) {
    edge_row  <- row_1
    edge_cmin <- min(col_1, col_2)
    edge_cmax <- max(col_1, col_2)

    if (edge_row <= rect$min_row || edge_row >= rect$max_row) {
      return(FALSE)
    }

    return(edge_cmax > rect$min_col && edge_cmin < rect$max_col)
  }

  # Vertical edge
  if (col_1 == col_2) {
    edge_col  <- col_1
    edge_rmin <- min(row_1, row_2)
    edge_rmax <- max(row_1, row_2)

    if (edge_col <= rect$min_col || edge_col >= rect$max_col) {
      return(FALSE)
    }

    return(edge_rmax > rect$min_row && edge_rmin < rect$max_row)
  }

  stop("not a straight edge")
}
