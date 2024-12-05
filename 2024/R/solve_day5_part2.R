#' Find the middle number of unsorted lists
#'
#' Given a sort of page ordering pairs x|y where y is after x. Store each y
#' against the same x in an environment (as a hashmap).
#'
#' Given lists of page updates, check each page for breaching the ordering rules
#' stored in the hashmap. For any updates that *do* breach, we find its middle
#' page number and sum them.
#'
#' @param input
#' Character vector of lines read from input. It has two sections - page
#' ordering pairs and lists of page numbers.
#'
#' @return
#' The sum of middle numbers of unsorted page update lists.
solve_day5_part2 <- function(input) {
  sep <- which(nchar(input) == 0)

  # ordering rules map ----
  ordering <- input[1:(sep-1)]
  ordering <- strsplit(ordering, "\\|")

  # using an environment as a hash map
  after_map <- new.env()

  # for each x|y pair
  # store y in the map against x as the key
  for (i in seq_along(ordering)) {
    pair <- ordering[[i]]
    key <- pair[[1]]
    value <- pair[[2]]

    # we don't need to sort here, but I did for some quick visual scanning
    after_map[[key]] <- sort(c(after_map[[key]], value))
  }

  # pages to produce in each update ----
  updates <- input[(sep+1):length(input)]
  updates <- strsplit(updates, ",")


  # check and extract ----
  # storage for middle page numbers
  middle_nums <- numeric(length(updates))

  # iterate to find lists which aren't sorted
  for (i in seq_along(updates)) {
    update <- updates[[i]]

    if (!is_sorted(updates[[i]], after_map)) {
      # then sort them and store their middle page number
      update <- sort_update(update, after_map)


      middle_index <- ceiling(length(update) / 2)
      middle_nums[[i]] <- as.numeric(update[[middle_index]])
    }
  }

  sum(middle_nums)
}

#' Whether a sequence of pages is in order
#'
#' Working backwards from the last page in the update to the first. Check
#' the pages before it in the list against that pages after_map list. If
#' any page is before when it should be after, the list is not sorted
#'
#' @param update
#' Character vector of page numbers
#' @param after_map
#' Environment, key-value pairs where each value is a character vector of pages
#' that should come after the key
#'
#' @return
#' Logical whether the pages are in order
is_sorted  <- function(update, after_map) {
  for (i in seq(length(update), 1, -1)) {
    check_num <- update[[i]]
    to_check <- update[i:1]

    if (any(to_check %in% after_map[[check_num]])) {
      return(FALSE)
    }
  }

  TRUE
}

#' Sort a list of pages
#'
#' Iterate through each number in the list: Check the preceding numbers, if any
#' of those are in the after list of the number being checked, move them after
#' that number and move the rest in front.
#'
#' @param update
#' A character vector of page numbers to sort
#' @param after_map
#' #' Environment, key-value pairs where each value is a character vector of
#' pages that should come after the key
#'
#' @return
#' A sorted version of `update`
sort_update <- function(update, after_map) {
  been_sorted <- character()

  while (length(been_sorted) != length(update)) {
    for (i in seq_along(update)) {
      check_num <- update[[i]]
      to_check <- update[-i]

      # location of numbers before the check_num that shouldn't be
      ooo <- which(to_check %in% after_map[[check_num]])

      # if we have any out of order
      if (length(ooo) > 0) {
        # move those numbers after the check_num
        # place the rest before the check_num to preserve ordering
        update <- c(to_check[-ooo], check_num, to_check[ooo])
      }

      been_sorted <- unique(c(been_sorted, check_num))
    }
  }

  update
}
