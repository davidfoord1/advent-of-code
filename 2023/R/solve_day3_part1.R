solve_day3_part1 <- function(input) {
  # find and extract numbers in each line
  num_locs <- stringi::stri_locate_all(input, regex = "\\d+")
  nums     <- stringi::stri_extract_all(input, regex = "\\d+")

  # find symbol locations in each line
  sym_locs <- stringi::stri_locate_all(input,
                                       regex = "(?![.|\\d]).")

  sym_locs <- lapply(sym_locs, \(x) x[, 1])

  # list to store which numbers are part numbers
  num_check <- vector("list", length(input))

  # for each line with numbers
  for (row in seq_along(num_locs)) {
    # skip rows with no numbers
    if (is.na(num_locs[[row]][[1]])) next

    # for each number in the line
    for (num in seq_len(NROW(num_locs[[row]]))) {
      num_loc <- num_locs[[row]][num, ]

      # search surrounding positions to confirm if the number
      # is a part number
      is_part <- is_part_number(row, num_loc, sym_locs)

      # store whether it is or not
      num_check[[row]] <- c(num_check[[row]], is_part)
    }

    # store NA if no part numbers found
    if (is.null(num_check[[row]])) {
      num_check[[row]] <- NA
    }
  }

  nums <- as.numeric(unlist(nums))
  num_check <- unlist(num_check)

  # sum any numbers where is_part_number returned TRUE
  sum(nums[num_check], na.rm = TRUE)
}

is_part_number <- function(row_loc, num_loc, sym_locs) {
  # columns to check start before and after the number
  col_range <- (num_loc[[1]] - 1):(num_loc[[2]] + 1)

  # check rows from the previous row to the next row
  for (row in (row_loc - 1):(row_loc + 1)) {
    # skipping out of bounds
    if (row <= 0) next
    if (row > NROW(sym_locs)) break

    # we have a part number if the checking range
    # intersects with a symbol position
    if (any(col_range %in% sym_locs[[row]])) {
      return(TRUE)
    }
  }

  FALSE
}
