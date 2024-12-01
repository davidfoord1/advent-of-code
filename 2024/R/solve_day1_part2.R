#' Score the similarities between 2 lists
#'
#' Parse the input text as 2 lists of numbers. Count the number of occurrences
#' of each item in the first list in the second list. Multiply each number by
#' its count to get the similarity score. Finally sum the score.
#'
#' To count I used the sum of equality between each number in the second
#' list and the number being counted from the first list.
#'
#' @param input
#' A character vector with 2 numbers per string
#'
#' @return
#' The sum of absolute differences between the sorted lists
solve_day1_part2 <- function(input) {
  # extract to 2 vectors for ease of operations
  lists <- strsplit(input, " +")
  first_list <- lapply(lists, \(x) x[[1]])
  second_list <- lapply(lists, \(x) x[[2]])

  first_list <- as.numeric(first_list)
  second_list <- as.numeric(second_list)

  # This time we need to count how many times a number in the first list
  # occurs in the second list. We can do that as the sum of logical equality
  # between each number in the second list and the number being counted from
  # the first list.
  counts <- vapply(first_list, \(num) sum(second_list == num), numeric(1))

  similarity_scores <- first_list * counts

  sum(similarity_scores)
}