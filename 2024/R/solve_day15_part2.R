solve_day15_part2 <- function(input) {
  move_start <- which.max(grepl("<", input))

  grid <- input[1L:(move_start-2L)]
  nrows <<- length(grid)
  grid <- matrix(unlist(strsplit(grid, "")), nrow = nrows, byrow = TRUE)

  # expand grid
  grid <- expand_grid(grid)

  moves <- input[move_start:length(input)]
  moves <- unlist(strsplit(paste0(moves, collapse = ""), ""))

  dirs <- list("^" = c(-1L, 0L),
               ">" = c(0L, 1L),
               "v" = c(1L, 0L),
               "<" = c(0L, -1L))

  final_grid <- Reduce(\(x, y) move_bot(x, y, dirs), moves, init = grid)

  gps_sum(final_grid)
}

move_bot <- function(grid, move, dirs) {
  bot_pos <- which(grid == "@", arr.ind = TRUE)

  dir <- dirs[[move]]
  next_pos <- bot_pos + dir
  next_tile <- grid[next_pos]

  if (next_tile == "#") {
    return(grid)
  }

  if (next_tile == ".") {
    grid[bot_pos] <- "."
    grid[next_pos] <- "@"

    return(grid)
  }

  grid <- push_boxes(grid, bot_pos, dir)

  grid
}

# now needs to (recursively?) branch out to box widths
# actually horizontal moves remain very similar
# but vertical moves can be massively branching

# find both box locations based on whether next is "[" or "]"
# trying to push up/down from both positions
push_boxes <- function(grid, bot_pos, dir) {
  # while next pos is "O" find the next pos
  is_horizontal <- abs(dir[[2L]]) == 1

  if (is_horizontal) {
    box_chain <- horizontal_box_chain(grid, bot_pos, dir)
  } else {
    box_chain <- vertical_box_chain(grid, bot_pos, dir)
  }

  # no positions to move (must have ran into "#")
  if (is.null(box_chain)) {
    return(grid)
  }

  if (!is_horizontal) {
    dir_is_up <- dir[[1L]] == -1L
    # order last to first in direction?
    rows <- vapply(box_chain, \(x) x[[1L]], numeric(1))
    order <- order(rows, decreasing = dir_is_up)

    box_chain <- box_chain[order]

  }
  # do it with second grid instead?
  new_grid <- grid
  for (pos in rev(box_chain)) {
    new_grid[pos + dir] <- grid[pos]
    new_grid[pos] <- "."

  }

  grid <- new_grid
  grid[bot_pos + dir] <- "@"
  grid[bot_pos] <- "."

  grid
}

horizontal_box_chain <- function(grid, bot_pos, dir) {
  next_pos <- bot_pos + dir
  box_chain <- vector("list", nrows)
  pos_ind <- 1L

  while(grid[next_pos] == "[" | grid[next_pos] == "]") {
    box_chain[pos_ind] <- list(next_pos)
    next_pos <- next_pos + dir
    pos_ind <- pos_ind + 1L
  }

  # if next pos is "#" return no positions to move
  if (grid[next_pos] == "#") {
    return(NULL)
  }

  # remove empty preallocation
  not_empty <- vapply(box_chain, \(x) !is.null(x), logical(1L))
  box_chain[not_empty]
}

vertical_box_chain_q <- function(grid, pos, dir) {
  queue <- new.env(parent = emptyenv())

}

vertical_box_chain <- function(grid, pos, dir) {
  # interacting with half a box at a time
  half_pos1 <- pos + dir
  half_tile1 <- grid[half_pos1]

  half_dir <- switch(half_tile1,
                     "[" = c(0L, 1L),
                     "]" = c(0L, -1L))

  half_pos2 <- half_pos1 + half_dir

  next_pos1 <- half_pos1 + dir
  next_pos2 <- half_pos2 + dir

  next_tile1 <- grid[next_pos1]
  next_tile2 <- grid[next_pos2]

  # if there is an obstacle the box can't move
  if (next_tile1 == "#" || next_tile2 == "#") {
    return(NULL)
  }

  box_chain <- c(list(half_pos1), list(half_pos2))

  # if both has free space
  if (next_tile1 == "." && next_tile2 == ".") {
    return(box_chain)
  }

  # change from depth first to breadth first?
  # -----------------

  # if either has a box to push, find the chain for that box
  if (next_tile1 != ".") {
    half_chain1 <- vertical_box_chain(grid, box_chain[[1L]], dir)

    if (is.null(half_chain1)) {
      return(NULL)
    }

    box_chain <- c(box_chain, half_chain1)
  }

  if (next_tile2 != ".") {
    half_chain2 <- vertical_box_chain(grid, box_chain[[2L]], dir)

    if (is.null(half_chain2)) {
      return(NULL)
    }

    box_chain <- c(box_chain, half_chain2)
  }

  unique(box_chain)
}

gps_sum <- function(grid) {
  boxes <- which(grid == "[", arr.ind = TRUE)
  # offset from border
  boxes <- boxes - 1L

  sum(boxes[, 1L] * 100 + boxes[, 2L])
}

expand_grid <- function(grid) {
  ncols <- NCOL(grid)

  new_grid <- vector("list", nrows)

  for(row in seq_len(nrows)) {
    new_row <- list(ncols)

    for (col in seq_len(ncols)) {
      tile <- grid[row, col]
      tile <- switch(tile,
                     "#" = c("#", "#"),
                     "O" = c("[", "]"),
                     "." = c(".", "."),
                     "@" = c("@", "."))

      new_row[[col]] <- tile
    }

    new_grid[[row]] <- unlist(new_row)
  }

  matrix(unlist(new_grid), nrow = nrows, byrow = TRUE)
}