solve_day5_part2 <- function(input) {
  ## extract seeds ----

  seed_ranges <- stringi::stri_extract_all(input[[1]], regex = "\\d+ \\d+")
  seed_ranges <- stringi::stri_split(unlist(seed_ranges), fixed = " ")
  seed_ranges <- lapply(seed_ranges, as.numeric)

  length_to_end <- function(range) {
    range[[2]] <- range[[1]] + range[[2]] - 1

    range
  }

  seed_ranges <- lapply(seed_ranges, length_to_end)

  ## extract maps ----

  map_input <- input[-1]
  # split input into sections based on blank line
  section_ends <- which(stringi::stri_detect(map_input, regex = "^$"))
  section_ends <- c(section_ends, length(map_input) + 1)

  map_range <- vector("list", length(section_ends) - 1)

  for (index in 1:(length(section_ends) - 1)) {
    map_range[index] <- list(c(section_ends[index], section_ends[index + 1]))
  }

  # each map goes from a blank line + 2 (skip map title)
  # to the next blank line - 1
  maps <- lapply(map_range,
                 \(range, m_i) m_i[ (range[[1]] + 2):(range[[2]] - 1)],
                 map_input)

  # pull out the numbers in each line, with lines nested in maps
  maps <- lapply(maps,
                 \(map) stringi::stri_extract_all(map, regex = "\\d+"))

  # convert nested character numbers to numeric
  maps <- lapply(maps, \(map) lapply(map, as.numeric))

  ## asdfasdf ----

  smallest_locations <- lapply(seed_ranges, find_smallest_in_range, maps)

  min(unlist(smallest_locations))
}

find_smallest_in_range <- function(input_range, maps, map_no = 1) {
  if (map_no > length(maps)) {
    return(unlist(input_range[[1]]))
  }

  map <- maps[[map_no]]

  ranges_to_check <- list(input_range)
  output_ranges <- list()

  for (mapping in map) {
    if (length(ranges_to_check) <= 0) {
      break
    }

    # remaining to check against next mapping
    remaining_ranges <- list()

    # mapping range
    dest_start   <- mapping[[1]]
    mapping_start <- mapping[[2]]
    length       <- mapping[[3]]
    mapping_end   <- (mapping_start + length) - 1


    for (range_to_check in ranges_to_check) {
      # starting range-to-check
      checking_start <- range_to_check[[1]]
      checking_end <- range_to_check[[2]]


      # Check for intersection between input and mapping range
      # input start before, inside or after the mapping
      # each handled separately

      # Before ----
      # if any of the input is before the mapping range
      # store it as remaining to be checked again on the next mapping

      # Example:
      # input         |-----|
      # mapping          |-----|
      #
      # remaining     |--|

      if (checking_start < mapping_start) {
        before_end <- min(checking_end, mapping_start - 1)
        before_range <- c(checking_start, before_end)

        remaining_ranges <- c(remaining_ranges, list(before_range))
      }

      # After ----
      # if any of the input is after the mapping range
      # store it as remaining to be checked again on the next iteration

      # Example:
      # input               |-------|
      # mapping          |-----|
      #
      # remaining              |----|

      if (checking_end > mapping_end) {
        after_start <- max(checking_start, mapping_end + 1)
        after_range <- c(after_start, checking_end)

        remaining_ranges <- c(remaining_ranges, list(after_range))
      }


      # Intersection ----
      # where there is an intersection derive the intersection range
      # then map it to the destination range
      # append the new range to the output ranges

      # Example:
      # input       |------|
      # mapping         |-----|
      # map-dest                 |-----|
      #
      # intersects      |--|
      # output                   |--|
      intersect_start <- max(checking_start, mapping_start)
      intersect_end <- min(checking_end, mapping_end)
      intersect_length <- intersect_end - intersect_start + 1

      if (intersect_length > 0) {
        mapped_start <- dest_start + (intersect_start - mapping_start)
        mapped_end <- mapped_start + intersect_length - 1
        output_ranges <- c(output_ranges, list(c(mapped_start, mapped_end)))

        intersecting_in_remaining <- vapply(remaining_ranges,
                                            \(x) identical(x, range_to_check),
                                            logical(1))


        remaining_ranges <- remaining_ranges[!intersecting_in_remaining]
      }

    }

    ranges_to_check <- remaining_ranges
  }

  output_ranges <- c(output_ranges, ranges_to_check)

  map_no <- map_no + 1

  # pass all output ranges back into this function as input
  # to be checked against the next group of mappings
  lapply(output_ranges, find_smallest_in_range, maps, map_no)
}
