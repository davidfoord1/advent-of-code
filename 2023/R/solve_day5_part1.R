solve_day5_part1 <- function(input) {
  ## extract seeds ----

  seeds <- stringi::stri_extract_all(input[[1]], regex = "\\d+")
  seeds <- as.numeric(unlist(seeds))

  ## extract maps ----

  map_input <- input[-1]
  # split input into sections based on blank line
  section_ends <- which(stringi::stri_detect(map_input, regex = "^$"))
  section_ends <- c(section_ends, length(map_input))

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

  ## map seeds to location ----

  # starting with the seed numbers, sequentially pass the numbers through
  # each source-to-destination map
  location <- Reduce(source_to_dest, maps, init = seeds)

  # lowest location number
  min(location)
}

source_to_dest <- function(source_nums, map) {
  vapply(source_nums, find_mapped_value, numeric(1), map)
}

find_mapped_value <- function(source_num, map) {
  for (mapping in map) {
    dest_start   <- mapping[[1]]
    source_start <- mapping[[2]]
    length       <- mapping[[3]]
    source_end   <- (source_start + length) - 1

    # if the number is within the source range
    if (source_num >= source_start & source_num <= source_end) {
      # return new destination number
      return(dest_start + source_num - source_start)
    }
  }

  # keep the number the same if no mapping found
  source_num
}