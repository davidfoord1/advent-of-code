# the same as part 1, just the assign_score function has changed
solve_day7_part2 <- function(input) {
  hands <- strsplit(input, " ")

  values <- vapply(hands, function(x) as.numeric(x[[2]]), numeric(1))
  hands <- vapply(hands, function(x) x[[1]], character(1))

  scores <- vapply(hands, assign_score, numeric(1))
  ranks <- rank(scores)

  sum(ranks * values)
}

assign_score <- function(hand, card_levels) {
  cards <- unlist(strsplit(hand, ""))

  # get a frequency table of cards to determine card points
  freq <- sort(table(cards), decreasing = TRUE)

  if ("J" %in% names(freq) && length(freq) > 1) {
    # add joker to the most frequent card that is not the joker
    if (names(freq)[[1]] != "J") {
      freq[[1]] <- freq[[1]] + freq[["J"]]
      freq[["J"]] <- 0
    } else {
      freq[[2]] <- freq[[2]] + freq[["J"]]
      freq[["J"]] <- 0
    }
  }

  # resort after joker adjustment
  freq <- sort(freq, decreasing = TRUE)

  # most significant scoring by most frequent card
  score <- freq[[1]] * 1e8

  # second most significant scoring by second most frequent card
  # for full house vs 3-of-a-kind and two-pair vs one-pair
  if (length(freq) > 1) {
    score <- score + freq[[2]] * 1e7
  }

  # J now lowest value
  card_names <- c("A", "K", "Q", "T", 9:2, "J")
  card_scores <- rev(seq_along(card_names))
  names(card_scores) <- card_names

  for (card_index in seq_along(cards)) {
    card_name <- cards[[card_index]]
    # weight cards earlier in the hand higher
    # from 14 ^ 4 down to 14 ^ 0
    score <- score + 14 ^ (5 - card_index)  * card_scores[[card_name]]
  }

  score
}
