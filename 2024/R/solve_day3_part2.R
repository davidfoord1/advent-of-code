#' Sum of enabled multiplications
#'
#' Add do() to the start of the input. Uses regex to split the input on do() and
#' don't(), removing all the sections that start with don't(). Then find the sum
#' of multiplying remaining pairs.
#'
#' @param input
#' Character vector of lines to parse for numbers to multiply.
#'
#' @return
#' The sum of results of enabled multiplications
solve_day3_part2 <- function(input) {
  line <- paste0(input, collapse = "")
  line <- paste0("do()", line)

  split <- stringi::stri_split_regex(line, "(?=do\\(\\))|(?=don't\\(\\))")
  split <- unlist(split)
  split <- split[substring(split, 1, 4) == "do()"]

  mul_sums <- lapply(split, sum_muls)

  sum(unlist(mul_sums))
}

#' Sum of multiplications
#'
#' Same as part 1 solution. Uses regex to find numbers in format mul(x, y).
#' Multiplies each pair and adds the results together.
#'
#' @param input
#' Character vector of lines to parse for numbers to multiply.
#'
#' @return
#' The sum of results of multiplications
sum_muls <- function(section) {
  muls <- stringi::stri_extract_all_regex(section,
                                          "(?<=mul\\()[0-9]+,[0-9]+(?=\\))")

  if (is.na(muls)) return(0)

  muls <- unlist(muls)
  muls <- strsplit(muls, ",")
  muls <- purrr::list_transpose(muls)
  muls <- lapply(muls, as.numeric)

  sum(muls[[1]] * muls[[2]])
}
