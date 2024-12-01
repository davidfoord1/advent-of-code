solve_day1_part1 <- function(input) {
  lists <- strsplit(input, " +")

  first_list <- lapply(lists, \(x) x[[1]])
  first_list <- sort(as.numeric(first_list))

  second_list <- lapply(lists, \(x) x[[2]])
  second_list <- sort(as.numeric(second_list))

  distances <- abs(second_list - first_list)

  sum(distances)
}