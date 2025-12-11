solve_day11_part1 <- function(input) {
  first_key <- "you"
  last_key <- "out"

  devices <- stringi::stri_extract_all_regex(input, r"{.+?(?=:)}")
  outputs <- stringi::stri_extract_all_regex(input, r"{(?<= )\w+}")
  n_devices <- length(input)

  # visited <- new.env(parent = emptyenv(), size = n_devices)
  queue <- vector("list", n_devices)
  queue_last <- queue_first <- 1L
  queue[[queue_last]] <- first_key

  path_count <- 0L

  while (queue_first <= length(queue)) {
    device <- queue[[queue_first]]
    pos <- which(devices == device)

    if (identical(device, "out")) {
      path_count <- path_count + 1L
    } else {
      next_devices <- outputs[[pos]]
      next_in_queue <- queue_last + seq_along(next_devices)
      queue[next_in_queue] <- next_devices
      queue_last <- queue_last + length(next_devices)
    }

    queue_first <- queue_first + 1L
  }

  path_count
}
