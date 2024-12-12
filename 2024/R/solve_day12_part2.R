# UNSOLVED ----
solve_day12_part2 <- function(input) {
  nrows <- length(input)
  grid <- matrix(unlist(strsplit(input, "")), nrow = nrows, byrow = TRUE)
  ncols <- NCOL(grid)

  plots <<- matrix(0L, nrows, ncols)
  plot_no <- 1L
  results <- vector("list", nrows)

  for (i in seq_len(nrows)) {
    for (j in seq_len(ncols)) {
      # if we find a plant not in plots
      if (plots[i, j] == 0L) {
        # start a search to build the plot
        results[[plot_no]] <- find_plot(i, j, plot_no)
        plot_no <- plot_no + 1L
      }
    }
  }

  results <- do.call(rbind, results)

  # area * perimeter
  sum(as.numeric(results[, 1]) * as.numeric(results[, 3]))
}

find_plot <- function(i, j, plot_no, prev_is_side = rep(FALSE, 4L), first = TRUE) {
  plant <- grid[i, j]
  plots[i, j] <<- plot_no

  dirs <- list(c(-1L, 0L), c(0L, 1L), c(1L, 0L), c(0L, -1L))
  area <- 1L
  # perimeter <- 4L

  is_side <- logical(4L)
  next_positions <- vector("list", 4L)

  edges <- matrix(0L, nrows + 2L, ncols + 2L)

  for (index in seq_along(dirs)) {
    dir <- dirs[[index]]
    adj_row <- i + dir[[1L]]
    adj_col <- j + dir[[2L]]

    # skip out of bounds
    if (adj_row <= 0L || adj_row > nrows || adj_col <= 0L || adj_col > ncols) {
      is_side[[index]] <- TRUE
      # TODO: store dir of edges!
      edges[adj_row+1L, adj_col+1L] <- 1L
      next
    }

    # different plants are not in the plot
    if (grid[adj_row, adj_col] != plant) {
      is_side[[index]] <- TRUE
      # TODO: store dir of edges!
      edges[adj_row+1L, adj_col+1L] <- 1L
      next
    }

    next_positions[[index]] <- c(adj_row, adj_col)
  }


  # how to carry over continued previous sides
  # while changing it when we "turn"
  sub_totals <- vector("list", 4L)

  for (index in seq_along(next_positions)) {
    next_pos <- next_positions[[index]]
    if (is.null(next_pos)) {
      next
    }

    adj_row <- next_pos[[1L]]
    adj_col <- next_pos[[2L]]
    # continue the search if we haven't been to that plant yet
    # do we need to have another for loop while we check for is_side?
    if (plots[adj_row, adj_col] == 0L) {
      sub_totals[[index]] <- find_plot(adj_row, adj_col, plot_no, is_side, FALSE)
    }
  }

  for (i in seq_along(sub_totals)) {
    totals <- sub_totals[[i]]

    if (length(totals) > 0 && !is.null(totals)) {
      area <- area + totals[[1]]
      edges <- edges + totals[[2]]
      # n_sides <- n_sides + totals[[2]]
    }
  }

  edge_count <- 0L

  if (first) {
    # calculate border along edges
    # edge_count <- rle(as.numeric(edges))[["values"]]
    # edge_count <- sum(edge_count)

    plot_edges <<- matrix(0L, nrows + 2L, ncols + 2L)
    edge_no <- 1L

    for (row in seq_len(NROW(edges))) {
      for (col in seq_len(NCOL(edges))) {
        if (edges[row, col] != 0L && plot_edges[row, col] == 0L) {
          mark_edge(row, col, edges, edge_no)
          edge_no <- edge_no + 1L
        }
      }
    }

    edge_count <- count_continuous_segments(edges)
  }

  list(area, edges, edge_count)
}

adjust_edge_counts <- function(plot_edges) {
  # Get unique edge numbers, excluding 0
  count <- max(plot_edges)
  # Initialize corrected edge count
  edges <- unique(as.numeric(plot_edges))
  edges <- edges[edges > 0]

  for (edge in edges) {
    # Get the positions of the current edge in the plot_edges grid
    positions <- which(plot_edges == edge, arr.ind = TRUE)

    # Calculate the row and column spans
    row_span <- range(positions[, 1])
    col_span <- range(positions[, 2])

    rows_spanned <- diff(row_span) + 1
    cols_spanned <- diff(col_span) + 1

    # If an edge spans multiple rows and columns, add one extra edge for each hidden edge
    if (rows_spanned > 1 && cols_spanned > 1) {
      print(edge)
      count <- count + 2
    }
  }

  count
}

count_continuous_sides <- function(grid, region) {
  rows <- nrow(grid)
  cols <- ncol(grid)

  # Initialize counts
  horizontal_continuous <- 0
  vertical_continuous <- 0

  # Check horizontally continuous sides
  for (i in 1:rows) {
    for (j in 1:(cols - 1)) {
      if (grid[i, j] == region && grid[i, j + 1] == region) {
        horizontal_continuous <- horizontal_continuous + 1
      }
    }
  }

  # Check vertically continuous sides
  for (j in 1:cols) {
    for (i in 1:(rows - 1)) {
      if (grid[i, j] == region && grid[i + 1, j] == region) {
        vertical_continuous <- vertical_continuous + 1
      }
    }
  }

  # Total continuous sides
  total_continuous <- horizontal_continuous + vertical_continuous
  return(total_continuous)
}

count_continuous_segments <- function(mat) {
  n <- nrow(mat)
  m <- ncol(mat)
  total_segments <- 0

  for (i in 1:n) {
    row <- mat[i, ]
    in_segment <- FALSE

    for (j in 1:m) {
      if (row[j] > 0) {
        if (!in_segment) {
          # Start of a new segment
          total_segments <- total_segments + 1
          in_segment <- TRUE
        }
      } else {
        in_segment <- FALSE
      }
    }
  }
  return(total_segments)
}
