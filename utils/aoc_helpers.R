# Input handling ----------------------------------------------------------

#' @params input A character vector resulting from a [readLines()] call on the
#'   input
aoc_input_to_chr_matrix <- function(input, sep = "") {
  strsplit(input, sep) |>
    stringi::stri_list2matrix(by_row = TRUE)
}

#' @params sample Text copied from the web page example/test inputs into a
#'   string
aoc_copied_sample_to_input <- function(sample) {
  strsplit(sample, "\n")[[1]]
}


# 2D nav ------------------------------------------------------------------

#' @return List of 2D directions specified as length 2 numeric vectors as
#'   (row, col).
aoc_2D_dirs <- function(horizontal = TRUE, vertical = TRUE, diagonal = TRUE) {
  up <- c(-1, 0)
  down <- c(1, 0)
  left <- c(0, -1)
  right <- c(0, 1)

  dirs <- list(
    "up" = up,
    "up-right" = up + right,
    "right" = right,
    "down-right" = down + right,
    "down" = down,
    "down-left" = down + left,
    "left" = left,
    "up-left" = up + left
  )

  if (!horizontal) {
    dirs["left"] <- dirs["right"] <- NULL
  }
  if (!vertical) {
    dirs["up"] <- dirs["down"] <- NULL
  }

  if (!diagonal) {
    dirs["up-left"] <- NULL
    dirs["up-right"] <- NULL
    dirs["down-left"] <- NULL
    dirs["down-right"] <- NULL
  }

  dirs
}

#' Move an agent on its path
#'
#' Giving a starting position and direction, takes a step in that direction when
#' possible. If there is an obstacle, instead cycle directions, through a
#' supplied list of directions. For example, turn 90 degrees whenever an
#' obstacle is encountered. This will continue until an end condition is met:
#' exiting the grid, entering a loop or reaching some target location.
#'
#' Constructed with 2024 Day 6 in mind.
#'   Part 1: A single run of this function, counting the marked squares in
#'     the output grid.
#'   Part 2: A run of this function with a different grid for each potential
#'    obstacle location, taking the sum of runs where the output value
#'    looped is true.
#'
#' TODO: Conditional movement based on character at current position. Like
#'   puzzles that have mirrors / | \ - dictating direction. So instead of
#'   supplying a set of `valid` and `obstacles` characters, have it possible
#'   to supply functions acting on (grid, from, to, dir, state).
#' TODO: Change to support 3D?
#' TODO: Change to support multiple agents?:
#'   Require each argument to be a list and then cycle through them.
#'
#' @param grid Character matrix to traverse.
#' @param pos Numeric starting position in matrix.
#' @param curr_dir Numeric starting direction.
#' @param dir_list List of numeric directions to cycle through. The default is
#'   both vertical and both horizontal directions.
#' @param dir_cycle Numeric directions to cycle through. If using the default
#'   `dir_list` then the default is to cycle through 90 degrees clockwise.
#' @param step_distance How far to travel on each step, that is, the direction
#'   is multiplied by this for each step.
#' @param marker Character to mark path on `grid` for checks.
#' @param valid Characters in `grid` that are valid positions to move to. By
#'   default this is the starting location and the commonly used "." character.
#' @param obstacles Characters in `grid` that are not valid positions to move
#'   to, so the agent will rotate instead of moving to these positons. By
#'   default "#"
#' @param target TODO: Set a point on the grid as a target to count as path
#'   completion. Can be as a numeric vector specifying a position in
#'   `grid`, or a character vector to check the contents of each position
#'   against. Characters will be added to list of `valid` characters.
#'
#' @return List of information about the complete path:
#'  - `grid`: input `grid` marked by `marker` where traversed.
#'  - `visited`: integer array of dimenions grid x number of directions tracking
#'   the number of time a state has been visited
#'  - `grid_exit`: integer for exiting the grid/reaching the grid bounds
#'  - `looped: intger for looping (reaching the same state twice)
#'  - `target`: intger for reaching the specified target
aoc_2D_path_try_dirs <- function(
  grid,
  pos,
  curr_dir,
  dir_list = aoc_2D_dirs(diagonal = FALSE),
  dir_cycle = 1L,
  step_distance = 1L,
  marker = "X",
  valid = c(marker, grid[pos], "."),
  obstacles = c("#"),
  exit = c("end", "teleport", "reflect"),
  loop_detect = c("state", "position"),
  target = NULL
) {
  if (!is.array(grid) | !is.character(grid)) {
    stop("`grid` must be a character array")
  }

  if (!is.matrix(pos) | !is.numeric(pos)) {
    stop("`pos` must be a numeric matrix")
  }

  if (!is.numeric(curr_dir)) {
    stop("`curr_dir` must be numeric")
  }

  if (!curr_dir %in% dir_list) {
    stop("`curr_dir` not found in dir_list")
  }

  exit <- match.arg(exit)
  loop_detect <- match.arg(loop_detect)

  # Starting intention boolean, but counter may be good
  grid_exit <- 0L
  looped <- 0L
  found_target <- 0L

  grid_dim <- dim(grid)
  nrows <- grid_dim[[1L]]
  ncols <- grid_dim[[2L]]

  dir_num <- match(list(curr_dir), dir_list)
  dirs_length <- length(dir_list)

  # Store state as an array with the dimensions of the grid and a location for
  # each dimension. This has a high memory cost but probably favourable
  # runtime-wise compared to growing a list/map with each new visited state.
  # Expand to facilitate more states than position and direction?
  state_dim <- c(grid_dim, length(dir_list))
  visited <- array(0L, state_dim)

  # TODO: add loop condition for arrived at same row, col with same direction
  # Require 3D visited array instead of matrix?
  repeat {
    grid[pos] <- marker
    state <- matrix(c(pos, dir_num), nrow = 1)
    visited[state] = visited[state] + 1L

    if (visited[state] > 1L) {
      looped <- TRUE
      break
    }

    next_pos <- pos + step_distance * curr_dir

    # stop repeating if reached out of bounds
    oob <- (aoc_2D_out_of_bounds(next_pos, nrows, ncols))

    if (oob) {
      grid_exit = TRUE
      break
    }

    # Cycle until next unobstructed direction
    while (grid[next_pos] %in% obstacles) {
      dir_num <- dir_num %% dirs_length + dir_cycle
      curr_dir <- dir_list[[dir_num]]

      next_pos <- pos + step_distance * curr_dir
      oob <- aoc_2D_out_of_bounds(next_pos, nrows, ncols)
      if (oob) break
    }

    # stop repeating if reached out of bounds
    if (oob) {
      grid_exit = TRUE
      break
    }

    stopifnot(grid[next_pos] %in% valid)

    pos <- next_pos
  }

  # list of visited counter
  # and other status info? like:
  #   exit out of bounds TRUE/FALSE
  #   found loop TRUE/FALSE
  #   found target location TRUE/FALSE
  list(
    grid = grid,
    visited = visited,
    grid_exit = grid_exit,
    looped = looped,
    found_target = found_target
  )
}

#' TODO: convert to take dims instead of nrows and ncols, so it can work
#' for any number of dimensions
aoc_2D_out_of_bounds <- function(pos, nrows, ncols) {
  pos[[1L]] < 1L ||
    pos[[1L]] > nrows ||
    pos[[2L]] < 1L ||
    pos[[2L]] > ncols
}
