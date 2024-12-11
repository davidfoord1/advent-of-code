solve_day11_part1 <- function(input, blinks = 25L) {
  all_stones <- vector("list", blinks+1L)
  all_stones[[1L]] <- as.numeric(unlist(strsplit(input, " ")))

  for (i in 2L:(blinks+1L)) {
    all_stones[[i]] <- unlist(lapply(all_stones[[i-1]], next_stone))
  }

  length(all_stones[[blinks+1L]])
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