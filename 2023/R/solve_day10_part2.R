solve_day10_part2 <- function(input) {
  start_row <- which(grepl("S", input))
  start_col <- as.integer(regexpr("S", input[start_row]))

  grid <- unlist(strsplit(input, ""))
  grid <- t(matrix(grid, nrow = length(input), ncol = nchar(input[[1]])))

  dir_ref <- list("N" = c(-1, 0), "S" = c(1, 0), "E" = c(0, 1), "W" = c(0, -1))

  pipe_ref <- list(
      "|" = c("N", "S"),
      "-" = c("E", "W"),
      "L" = c("N", "E"),
      "J" = c("N", "W"),
      "7" = c("S", "W"),
      "F" = c("S", "E"),
      "." = NA
    )

  # find start ----

  dir <- numeric()
  start_dirs <- character()

  for (check_dir in dir_ref) {
    row <- start_row + check_dir[[1]]
    col <- start_col + check_dir[[2]]

    pipe <- grid[row, col]
    pipe_dirs <- dir_ref[ pipe_ref[[pipe]] ]

    # check for opposite direction
    opp_dir <- list(-c(check_dir[[1]], check_dir[[2]]))

    if (opp_dir %in% pipe_dirs) {
      # this time we need to know what type of pipe the start is
      # so instead of `break`ing when we find a connection
      # we store the direction as N, E, S or W and keep searching
      dir_no <- match(list(check_dir), dir_ref)
      dir_name <- names(dir_ref)[[dir_no]]

      start_dirs <- c(start_dirs, dir_name)

      # we still want the dir to move in
      dir <- unlist(pipe_dirs[-match(opp_dir, pipe_dirs)])
    }
  }

  # identify starting pipe from it's directions
  # ordering to match pipe_ref, because S is last in our search
  start_dirs <- as.character(factor(start_dirs, levels = c("N", "E", "S", "W")))

  pipe_no <- match(list(start_dirs), pipe_ref)
  pipe_symbol <- names(pipe_ref)[[pipe_no]]

  grid[start_row, start_col] <- pipe_symbol

  # follow the connection
  steps <- 1

  while (row != start_row | col != start_col) {
    # mark the current position as in the loop
    grid[row, col] <- paste0(grid[row, col], "*")

    # move to next position
    row <- row + dir[[1]]
    col <- col + dir[[2]]

    steps <- steps + 1

    # find the direction opposite to the one we moved in
    opp_dir <- list(-c(dir[[1]], dir[[2]]))

    # find the 2 directions for the next pipe
    pipe <- grid[row, col]
    pipe_dirs <- dir_ref[ pipe_ref[[pipe]] ]

    # the next direction is the one that's not opposite to the previous one
    dir <- unlist(pipe_dirs[-match(opp_dir, pipe_dirs)])
  }

  # going along each row marking whether a tile is "O" outisde or "I" inside
  # the loop, flipping whenever we've cross the border of the loop

  # 1. The simpler case - meeting a pipe completely perpendicular to your
  # direction of travel. Because I am traversing along columns by row, `|` is
  # perpendicular and I ignore `-` because it is parellel to my travel.

  # OOO|III|OOO ~ Flipping on clear border

  # 2. For the more complex case of corners:
  # Count only those pointing in one just one of the perpendicular directions,
  # so that we only mark inside when corners pointing in opposite directions
  # connect. For example:

  # OOOL---JOOO ~ Flip even number of times (twice) remains outside

  # OOOF---7OOO ~ Flip even number of times (zero) remains outside

  # OOOL---7III ~ Flip odd number of times (once) goes inside

  # OOOF---JIII ~ Flip odd number of times (once) goes inside

  for (row in seq_len(NROW(grid))) {
    marker <- "O"

    for (col in seq_len(NCOL(grid))) {
      # if a tile is not part of the loop we mark it
      if (!grepl("*", grid[row, col], fixed = TRUE)) {
        grid[row, col] <- marker
      }

      # if meet |, L or J we flip the marker
      if (grepl("[\\||L|J]\\*", grid[row, col])) {
        marker <- switch(marker,
                         "O" = "I",
                         "I" = "O")
      }
    }
  }

  sum(grid == "I")
}
