solve_day6_part1 <- function(input) {
  races <- stringi::stri_extract_all(input[[1]], regex = "\\d+")
  races <- as.numeric(unlist(races))

  records <- stringi::stri_extract_all(input[[2]], regex = "\\d+")
  records <- as.numeric(unlist(records))

  ways_to_win <- numeric(length(races))

  for (index in seq_along(races)) {
    distances <- vector("list", length(races[[index]]))

    for (hold_time in seq_len(races[[index]])) {
      distances[[hold_time]] <- hold_time * (races[[index]] - hold_time)
    }

    ways_to_win[[index]] <- sum(distances > records[[index]])
  }


  Reduce(`*`, ways_to_win)
}
