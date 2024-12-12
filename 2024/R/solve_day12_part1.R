solve_day12_part1 <- function(input) {
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
        results[[plot_no]] <- find_plot(i, j, plot_no, environment())
        plot_no <- plot_no + 1L
      }
    }
  }

  results <- do.call(rbind, results)

  # area * perimeter
  sum(results[, 1] * results[, 2])
}

# should this function return (area, perimeter)?
find_plot <- function(i, j, plot_no, envir) {
  plant <- grid[i, j]
  plots[i, j] <<- plot_no

  dirs <- list(c(-1L, 0L), c(0L, 1L), c(1L, 0L), c(0L, -1L))
  area <- 1L
  perimeter <- 4L
  sub_totals <- list()

  for (index in seq_along(dirs)) {
    dir <- dirs[[index]]
    adj_row <- i + dir[[1L]]
    adj_col <- j + dir[[2L]]

    # skip out of bounds
    if (adj_row <= 0L || adj_row > nrows || adj_col <= 0L || adj_col > ncols) {
      next
    }

    # skip non-plot members
    if (grid[adj_row, adj_col] != plant) {
      next
    }

    perimeter <- perimeter - 1L

    # continue the search if we haven't been to that plant yet
    if (plots[adj_row, adj_col] == 0L) {
      sub_totals[[index]] <- find_plot(adj_row, adj_col, plot_no, envir)
    }
  }

  for (i in seq_along(sub_totals)) {
    totals <- sub_totals[[i]]

    if (length(totals) > 0) {
      area <- area + totals[[1]]
      perimeter <- perimeter + totals[[2]]
    }
  }

  c(area, perimeter)
}
