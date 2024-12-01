#' Compare the differences between 2 lists
#'
#' A friendly opener! We parse the input text as 2 lists of numbers and sort
#' them. Then we take the sum of the absolute differences between pairs.
#'
#' @param input
#' A character vector with 2 numbers per string
#'
#' @return
#' The sum of absolute differences between the sorted lists
solve_day1_part1 <- function(input) {
  # extract to 2 vectors for ease of operations
  lists <- strsplit(input, " +")
  first_list <- lapply(lists, \(x) x[[1]])
  second_list <- lapply(lists, \(x) x[[2]])

  # sort so that we pair up the smallest number in the left list with the
  # smallest number in the right list etc.
  first_list <- sort(as.numeric(first_list))
  second_list <- sort(as.numeric(second_list))

  # we want the absolute distance
  # it doesn't matter which list has the larger number
  distances <- abs(second_list - first_list)

  sum(distances)
}