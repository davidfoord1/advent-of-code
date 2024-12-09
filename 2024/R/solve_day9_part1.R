solve_day9_part1 <- function (input) {
  lengths <- as.integer(unlist(strsplit(input, "")))

  id <- 0L
  is_file <- TRUE
  # disk <- character(length(lengths) * 9L)
  disk <- vector("list", length(lengths))

  for (i in seq_along(lengths)) {
    if (is_file) {
      disk[[i]] <- list(rep(id, lengths[[i]]))

      id <- id + 1L
      is_file <- FALSE
    } else {
      disk[[i]] <- list(rep(".", lengths[[i]]))

      is_file <- TRUE
    }
  }

  cont <- unlist(disk)

  space_ptr <- 1L
  space_ptr <- next_space(cont, space_ptr)
  block_ptr <- length(cont)
  block_ptr <- next_block(cont, block_ptr)

  while (!is.null(space_ptr) && space_ptr < block_ptr) {
    cont[[space_ptr]] <- cont[[block_ptr]]

    block_ptr <- block_ptr - 1L

    space_ptr <- next_space(cont, space_ptr)
    block_ptr <- next_block(cont, block_ptr)
  }

  cont <- cont[1:block_ptr]

  cont <- as.numeric(cont[cont != "."])
  sum(cont * 0L:(length(cont) - 1), na.rm = TRUE)
}

next_space <- function(disk, ptr) {
  for (i in ptr:length(disk)) {
    if (disk[[i]] == ".") {
      return(i)
    }
  }
}


next_block <- function(disk, ptr) {
  for (i in ptr:1L) {
    if (disk[[i]] != ".") {
      return(i)
    }
  }
}

