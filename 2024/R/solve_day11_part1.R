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
solve_day11_part1 <- function(input, blinks = 25L) {
  stones <- as.numeric(unlist(strsplit(input, " ")))

  for (i in 1L:(blinks)) {
    stones <- unlist(lapply(stones, next_stone))
  }

  length(stones)
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
  stopifnot(is.numeric(stone))

  if (stone == 0L) {
    return(1L)
  }

  chars <- nchar(stone)

  if (chars %% 2L == 0L) {
    half = chars / 2L

    stone1 <- substring(stone, 1L, half)
    stone2 <- substring(stone, (half+1L), chars)
    return(as.numeric(c(stone1, stone2)))
  }

  as.numeric(stone) * 2024L
}