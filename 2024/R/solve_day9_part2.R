solve_day9_part2 <- function (input) {
  lengths <- as.integer(unlist(strsplit(input, "")))

  id <- 0L
  is_file <- TRUE
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

  first_space <- 1L
  first_space <- next_space(cont, first_space)
  block_ptr <- length(cont)

  # ----

  while(first_space < block_ptr) {
    if (cont[[block_ptr]] == ".") {
      block_ptr <- block_ptr - 1L
      next
    }

    block_id <- cont[[block_ptr]]
    block_len <- 1L

    for (i in (block_ptr-1L):1L) {
      if (cont[[i]] != block_id) {
        break
      }

      block_len <- block_len + 1L
    }

    space_ptr <- first_space
    repeat {
      if (cont[[space_ptr]] != ".") {
        space_ptr <- space_ptr + 1L
        next
      }

      space_len <- 1L

      for (i in (space_ptr+1L):block_ptr) {
        if (cont[[i]] != ".") {
          break
        }

        space_len <- space_len + 1L
      }

      if (space_len >= block_len) {
        space_pos <- space_ptr:(space_ptr + block_len - 1L)
        block_pos <- block_ptr:(block_ptr - block_len + 1L)

        cont[space_pos] <- cont[block_pos]
        cont[block_pos] <- "."

        block_ptr <- block_ptr - block_len
        first_space <- next_space(cont, first_space)
        break
      } else {
        space_ptr <- space_ptr + 1L
        space_ptr <- next_space(cont, space_ptr)
      }

      if (space_ptr >= block_ptr) {
        block_ptr <- block_ptr - block_len
        break
      }
    }
  }

  cont[cont == "."] <-  0L
  cont <- as.numeric(cont)
  sum(cont * 0L:(length(cont) - 1L), na.rm = TRUE)
}

next_space <- function(disk, ptr) {
  for (i in ptr:length(disk)) {
    if (disk[[i]] == ".") {
      return(i)
    }
  }
}