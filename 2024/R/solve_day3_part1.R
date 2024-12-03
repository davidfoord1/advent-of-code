#' Sum of multiplications
#'
#' Uses regex to find numbers in format mul(x, y). Multiplies each pair and
#' adds the results together.
#'
#' @param input
#' Character vector of lines to parse for numbers to multiply.
#'
#' @return
#' The sum of results of multiplications
solve_day3_part1 <- function(input) {
  muls <- stringi::stri_extract_all_regex(input,
                                          "(?<=mul\\()[0-9]+,[0-9]+(?=\\))")

  muls <- unlist(muls)
  muls <- strsplit(muls, ",")
  muls <- purrr::list_transpose(muls)
  muls <- lapply(muls, as.numeric)

  sum(muls[[1]] * muls[[2]])
}
