solve_day4_part2 <- function(input) {
  cards <- strsplit(input, ":|\\|")

  # numbers between : and |
  winners <- lapply(cards, \(x) x[[2]])
  winners <- stringi::stri_extract_all(winners, regex = "\\d+")

  # numbers from | to the end of the line
  playing <- lapply(cards, \(x) x[[3]])
  playing <- stringi::stri_extract_all(playing, regex = "\\d+")

  # count playing numbers in winning numbers
  win_count <- Map(\(w, p) sum(p %in% w),
                   winners,
                   playing)

  win_count <- unlist(win_count)

  copies <-  rep(1, length(cards))

  # because cards only add new copies of later cards,
  # we can sequence along them in order
  for (card in seq_along(copies)) {
    # if there are winning numbers
    if (win_count[card] > 0) {
      # we add wins to the number of following cards
      new_card_range <- (card + 1):(card + win_count[[card]])
      # the number we add is the number of copies we have of the current card
      copies[new_card_range] <- copies[new_card_range] + copies[card]
    }
  }

  sum(copies)
}
