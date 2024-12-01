solve_day1_part2 <- function(input) {
  lists <- strsplit(input, " +")

  first_list <- lapply(lists, \(x) x[[1]])
  first_list <- sort(as.numeric(first_list))

  second_list <- lapply(lists, \(x) x[[2]])
  second_list <- sort(as.numeric(second_list))

  counts <- lapply(first_list, \(num) sum(second_list == num))
  counts <- as.numeric(counts)

  similarity_scores <- first_list * counts

  sum(similarity_scores)
}