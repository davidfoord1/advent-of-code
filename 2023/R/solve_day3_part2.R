solve_day3_part2 <- function(input) {
  # find and extract numbers in each line (in the same list structure)
  num_locs <- stringi::stri_locate_all(input, regex = "\\d+")
  nums     <- stringi::stri_extract_all(input, regex = "\\d+")

  # find "*" locations (column) in each line
  gear_locs <- stringi::stri_locate_all(input,
                                       regex = "\\*")

  # location has start and end, but they are length 1, so we need only one
  gear_locs <- lapply(gear_locs, \(x) x[, 1])

  ratio_sum <- 0

  # iterate through every `*` location
  for (row in seq_along(gear_locs)) {
    for (col in gear_locs[[row]]) {
      if (length(col) == 1 & is.na(col)) break # skip if no gears in line

      # search surrounding area for numbers
      # store their product if there are 2
      ratio_sum <- ratio_sum + find_gear_ratio(row, col, num_locs, nums)
    }
  }

  sum(ratio_sum)
}


find_gear_ratio <- function(row, col, num_locs, nums) {
  # 9 position grid search area around `*`
  search_rows <- (row - 1):(row + 1)
  search_cols <- (col - 1):(col + 1)

  # store potential gear numbers
  gear_nums <- numeric()

  for (search_row in search_rows) {
    # skipping out of bounds
    if (search_row <= 0) next
    if (search_row > NROW(num_locs)) break

    # iterate through the number locations in the search row
    row_nums <- num_locs[[search_row]]

    for (index in seq_len(NROW(row_nums))) {
      # get the column positions for the number
      number_cols <- row_nums[index, 1]:row_nums[index, 2]

      # if the number intersects the search area
      if (any(number_cols %in% search_cols)) {
        # add it to the list of potential gear numbers
        # take the actual number based on the current number location
        gear_num <- nums[[search_row]][[index]]

        gear_nums <- c(gear_nums, as.numeric(gear_num))
      }
    }
  }

  # if there are two numbers we've found a gear
  if (length(gear_nums) == 2) {
    # multiply numbers to get the gear ratio
    gear_nums[[1]] * gear_nums[[2]]
  } else {
    # found a `*` that wasn't a gear
    0
  }
}
