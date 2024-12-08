solve_day4_part1 <- function(input) {
  # split on : or |
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

  win_count <- as.numeric(unlist(win_count))
  # remove rows with no wins
  win_count <- win_count[win_count != 0]

  # start from 1 and double for each additional win
  sum(2 ^ (win_count - 1))
}