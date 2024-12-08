solve_day10_part1 <- function(input) {
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

  for (check_dir in dir_ref) {
    row <- start_row + check_dir[[1]]
    col <- start_col + check_dir[[2]]

    pipe <- grid[row, col]
    pipe_dirs <- dir_ref[ pipe_ref[[pipe]] ]

    # check for opposite direction
    opp_dir <- list(-c(check_dir[[1]], check_dir[[2]]))

    if (opp_dir %in% pipe_dirs) {
      dir <- unlist(pipe_dirs[-match(opp_dir, pipe_dirs)])
      break
    }
  }

  # follow the connection
  steps <- 1

  while (row != start_row | col != start_col) {
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

  # furthers pipe is half of the full loop
  steps / 2
}

