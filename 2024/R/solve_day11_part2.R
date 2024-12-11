#' Find the number of stones after many changes
#'
#' Given a set of stones with numbers on them, for the number of times specified
#' by `blinks`, apply a set of rules for how they will change
#'
#' Because we will end up with many stones having the same number, instead of
#' tracking each number individually, store a frequency table/hash map of counts
#' of each number.
#'
#' Return the final count.
#'
#' @param input
#' String of numbers (on stones)
#' @param blinks
#' Number of iterations to apply rules to each number
#'
#' @return
#' numeric(1) The number of stones after `blinks` iterations
solve_day11_part2 <- function(input, blinks = 75L) {
  initial_stones <- as.double(unlist(strsplit(input, " ")))
  stone_counts <- table(initial_stones)

  for (i in 1L:blinks) {
    # environment as hash table
    new_counts <- new.env(parent = emptyenv())

    for (stone in names(stone_counts)) {
      # apply the rules
      results <- next_stone(as.numeric(stone))
      results <- as.character(results)

      # need to specify as double because integers (32-bit) may overflow
      count <- as.double(stone_counts[[stone]])

      for (result in results) {
        if (exists(result, envir = new_counts, inherits = FALSE)) {
          new_counts[[result]] <- new_counts[[result]] + count
        } else {
          new_counts[[result]] <- count
        }
      }
    }

    # convert environment back to vector for next iteration
    stone_counts <- unlist(as.list(new_counts), use.names = TRUE)
  }

  sum(stone_counts)
}

#' Apply *change* to stone
#'
#'    0 becomes 1
#'    numbers with an even number of digits split in half
#'    everything else gets multiplied by 2024
#'
#' @param stone
#' numeric(1) Number to apply rules to
#'
#' @return
#' numeric(1) Number resulting from application of rules to `stone`
next_stone <- function(stone) {
  if (stone == 0) {
    return(1)
  }

  chars <- nchar(stone)
  # even number of digits
  if (chars %% 2L == 0L) {
    half = chars / 2L

    stone1 <- substring(stone, 1L, half)
    stone2 <- substring(stone, (half+1L), chars)
    return(as.double(c(stone1, stone2)))
  }

  stone * 2024
}
