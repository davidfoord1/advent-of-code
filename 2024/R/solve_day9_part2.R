#' Find the filesystem checksum
#'
#' Convert given lengths list alternating between files with ascending ID and
#' spaces - the full disk layout. Find the length of continous sections with
#' `rle`. Iterate down the file IDs, searching earlier on the disk (in the list)
#' to find a space of sufficient length to move the file to.
#'
#' Multiply each instance of the ID by its new position and sum the result.
#'
#' @param input
#' Single string of digits where they alternate between indicating the length of
#' a file and the length of free space.
#'
#' @return
#' numeric(1) Filesystem checksum
solve_day9_part2 <- function(input) {
  lens <- as.integer(unlist(strsplit(input, "")))
  lens_seq <- seq_along(lens)
  lens_len <- length(lens)

  # alternating lengths are files
  is_file <- lens_seq %% 2L == 1L

  # ids start from 0 and increase every 2 lengths
  ids <- (lens_seq - 1L) * is_file / 2L
  ids <- lapply(lens_seq, \(i) rep(ids[[i]], lens[[i]]))

  spaces <- rep(".", lens_len)
  spaces <- lapply(lens_seq, \(i) rep(spaces[[i]], lens[[i]]))

  disk <- ifelse(is_file, ids, spaces)
  disk <- unlist(disk, recursive = FALSE)

  space_rle <- rle(disk == ".")

  disk_rle <- rle(disk)

  # we take the lengths of the non-space values + 1
  # to get the start of the next space block
  space_starts <- cumsum(disk_rle[["lengths"]]) - lag(disk_rle[["lengths"]]) + 1L

  spaces <- data.frame(
    starts = space_starts[disk_rle[["values"]] == "."],
    lens = disk_rle[["lengths"]][disk_rle[["values"]] == "."]
  )

  file_ends <- cumsum(disk_rle[["lengths"]])
  file_ends <- file_ends[!disk_rle[["values"]] == "."]
  file_lens <- disk_rle[["lengths"]][!disk_rle[["values"]] == "."]
  file_vals <- disk_rle[["values"]][!disk_rle[["values"]] == "."]

  for (file in rev(seq_along(file_ends))) {
    file_end <- file_ends[[file]]
    file_len <- file_lens[[file]]

    # find space before the block and of sufficient length
    valid_spaces <- which(spaces[["starts"]] <= file_end &
                           spaces[["lens"]] >= file_len)

    if (length(valid_spaces) > 0) {
      valid_space <- valid_spaces[[1L]]

      space_start <- spaces[["starts"]][valid_space]
      space_len <-  spaces[["lens"]][valid_space]

      # move file to space
      disk[space_start:(space_start + file_len - 1L)] <- file_vals[[file]]

      # store remaining space
      new_len <- space_len - file_lens[[file]]
      spaces[["lens"]][valid_space] <- new_len

      if (new_len > 0L) {
        spaces[["starts"]][valid_space] <-
          spaces[["starts"]][valid_space] + file_len
      }

      # write space to old file location
      disk[file_end:(file_end - file_len + 1L)] <- "."
    }

  }

  disk[disk == "."] <- 0L
  disk <- as.numeric(disk)
  sum(disk * 0L:(length(disk) - 1L))
}

