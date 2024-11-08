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
  copy_no <- 1
  while (copy_no <= max(copies)) {
    for (card in seq_along(copies)) {
      if (copies[card] >= copy_no) {
        new_cards <- card + seq_len(win_count[card])
        copies[new_cards] <- copies[new_cards] + 1
      }
    }

    copy_no <- copy_no + 1
  }

  sum(copies)
}
