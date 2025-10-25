solve_day15_part1 <- function(input) {
  move_start <- which.max(grepl("<", input))

  grid <- input[1L:(move_start-2L)]
  nrows <<- length(grid)
  grid <- matrix(unlist(strsplit(grid, "")), nrow = nrows, byrow = TRUE)

  moves <- input[move_start:length(input)]
  moves <- unlist(strsplit(paste0(moves, collapse = ""), ""))

  dirs <- list("^" = c(-1L, 0L),
               ">" = c(0L, 1L),
               "v" = c(1L, 0L),
               "<" = c(0L, -1L))

  final_grid <- Reduce(\(x, y) move_bot(x, y, dirs),
                       moves,
                       init = grid)

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

  grid <- push_boxes(grid, bot_pos, dir)#

  grid
}

push_boxes <- function(grid, bot_pos, dir) {
  # while next pos is "O" find the next pos
  next_pos <- bot_pos + dir
  box_chain <- vector("list", nrows)
  pos_ind <- 1L

  while(grid[next_pos] == "O") {
    box_chain[pos_ind] <- list(next_pos)
    next_pos <- next_pos + dir
    pos_ind <- pos_ind + 1L
  }

  # if next pos is "#" return grid
  if (grid[next_pos] == "#") {
    return(grid)
  }

  # if next pos is "." move bot and boxes
  for (pos in box_chain) {
    grid[pos + dir] <- "O"
  }


  grid[bot_pos + dir] <- "@"
  grid[bot_pos] <- "."

  grid
}

gps_sum <- function(grid) {
  boxes <- which(grid == "O", arr.ind = TRUE)
  # offset from border
  boxes <- boxes - 1L

  sum(boxes[, 1L] * 100 + boxes[, 2L])
}
