solve_day5_part1 <- function(input) {
  sep <- which(nchar(input) == 0)

  ordering <- input[1:(sep-1)]
  ordering <- strsplit(ordering, "\\|")

  after_map <- new.env()

  for (i in seq_along(ordering)) {
    key <- ordering[[i]][[1]]
    value <- ordering[[i]][[2]]
    if (is.null(after_map[[key]])) {
      after_map[[key]] <- value
    } else {
      after_map[[key]] <- sort(c(after_map[[key]], value))
    }
  }

  updates <- input[(sep+1):length(input)]
  updates <- strsplit(updates, ",")

  middle_nums <- numeric(length(updates))

  for (i in seq_along(updates)) {
    update <- updates[[i]]
    if (is_ordered(updates[[i]], after_map)) {

      middle_index <- ceiling(length(update) / 2)

      middle_nums[[i]] <- update[[middle_index]]
    } else {

      update <- sort_update(update, after_map)
    }


  }

  sum(as.numeric(middle_nums))
}

is_ordered  <- function(update, after_map) {
  for (i in seq(length(update), 1, -1)) {
    check_num <- update[[i]]
    to_check <- update[i:1]

    if (any(to_check %in% after_map[[check_num]])) {
      return(FALSE)
    }
  }

  TRUE
}
