#' Find the total fence pricing
#'
#' Search through the garden plot grid, finding each continuous group of plants
#' where the character is the same in adjacent squares. For each group of
#' plants find the area (count of plants) and perimeter (count of length 1
#' edges).
#'
#' The price for each group is its area * perimeter. The sum across groups gives
#' the final total.
#'
#' @param input
#' Character vector of rows in garden plot grid
#'
#' @return
#' numeric(1) Total fence price
solve_day12_part1 <- function(input) {
  nrows <<- length(input)
  grid <- matrix(unlist(strsplit(input, "")), nrow = nrows, byrow = TRUE)
  ncols <<- NCOL(grid)

  # global store of visited plots
  plots <<- matrix(0L, nrows, ncols)
  plot_no <- 1L
  results <- vector("list", nrows)

  for (i in seq_len(nrows)) {
    for (j in seq_len(ncols)) {
      # if we find a plant not in plots
      if (plots[i, j] == 0L) {
        # start a search to build the plot
        results[[plot_no]] <- find_plot(i, j, plot_no, grid)
        plot_no <- plot_no + 1L
      }
    }
  }

  results <- do.call(rbind, results)

  # area * perimeter
  sum(results[, 1] * results[, 2])
}

#' Find every plant in a plot
#'
#' From the first plant in a group search in the surrounding four directions
#' for more of the same plant. Search recursively to find the full group.
#'
#' During the search, store the `plot_no` to the global plots matrix, and add
#' up the area and perimeters of each branch in the search.
#'
#' Where area is the total count i.e. 1 per plant.
#' Perimeter per plant is 4 minus the number of plants in the same group.
#'
#' @param i
#' numeric(1) grid row to search from
#' @param j
#' numeric(1) grid column to search from
#' @param plot_no
#' numeric(1) ID of current plant group
#'
#' @return
#' numeric(2) Total area and total perimeter for the group from this point
find_plot <- function(i, j, plot_no, grid) {
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
      sub_totals[[index]] <- find_plot(adj_row, adj_col, plot_no, grid)
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
