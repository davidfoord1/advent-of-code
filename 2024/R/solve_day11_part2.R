solve_day11_part2 <- function(input, blinks = 75L) {
  # Convert input into a named numeric vector where names are unique stones and values are counts
  initial_stones <- as.numeric(unlist(strsplit(input, " ")))
  stone_counts <- table(initial_stones)

  for (i in 1L:blinks) {
    print(i)
    # Create a new table to hold updated counts
    new_counts <- numeric()

    for (stone in names(stone_counts)) {
      count <- stone_counts[[stone]]
      results <- next_stone(as.numeric(stone))
      results <- as.character(results)

      for (result in results) {
        if (result %in% names(new_counts)) {
          new_counts[[result]] <- new_counts[[result]] + count
        } else {
          new_counts[[result]] <- count
        }
      }
    }

    # Update stone counts with new counts
    stone_counts <- new_counts
  }

  # Sum of counts gives the total number of stones
  sum(stone_counts)
}

next_stone <- function(stone) {
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
