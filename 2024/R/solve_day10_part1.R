solve_day10_part1 <- function(input) {
  grid <- input |>
    strsplit("") |>
    unlist() |>
    as.integer() |>
    matrix(nrow = length(input), byrow = TRUE)

  nrows <<- NROW(grid)
  ncols <<- NCOL(grid)

  trailheads <- which(grid == 0L, arr.ind = TRUE)

  trail_scores <- integer(NROW(trailheads))

  # Iterate over all trailheads
  for (i in seq_len(NROW(trailheads))) {
    # Create a new visited matrix for this trailhead
    visited <<- matrix(FALSE, nrow = nrows, ncol = ncols)
    # Count paths starting from the trailhead
    trail_scores[i] <- count_paths(trailheads[i, ], 0L, grid)
  }

  sum(trail_scores)
}


count_paths <- function(pos, height, grid) {
  # Unpack position
  row <- pos[1]
  col <- pos[2]

  # Base case: if already visited, return 0
  # part 1 only
  if (visited[row, col]) {
    return(0L)
  }

  # Mark current position as visited
  visited[row, col] <<- TRUE

  # If the current position has a value of 9, return 1 (found a unique 9)
  if (grid[row, col] == 9L) {
    return(1L)
  }

  # Define directions for movement (up, right, down, left)
  dirs <- list(c(-1L, 0L), c(0L, 1L), c(1L, 0L), c(0L, -1L))

  # Compute next positions
  next_pos <- lapply(dirs, \(dir) c(row, col) + dir)

  # Filter positions that are in bounds
  in_bounds <- vapply(next_pos, inbounds, logical(1))

  # Keep only valid next positions
  next_pos <- next_pos[in_bounds]

  next_heights <- vapply(next_pos,
                         \(pos) grid[pos[[1]], pos[[2]]],
                         integer(1L))


  # Filter by height: move only to positions with value = height + 1
  valid_trail <- next_heights == height + 1
  next_pos <- next_pos[valid_trail]

  # Recurse into valid next positions
  path_counts <- vapply(next_pos,
                       \(pos) count_paths(pos, height + 1L, grid),
                       numeric(1))


  # Return the total count of unique 9s
  sum(path_counts)
}

inbounds <- function(pos, grid) {
  pos <- unlist(pos)
  row <- pos[[1]]
  col <- pos[[2]]

  row > 0L && row <= nrows && col > 0L && col <= ncols
}
