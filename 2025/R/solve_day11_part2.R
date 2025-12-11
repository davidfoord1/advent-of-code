solve_day11_part2 <- function(input) {
  first_key <- "svr"
  last_key <- "out"

  devices <- stringi::stri_extract_all_regex(input, r"{.+?(?=:)}")
  outputs <- stringi::stri_extract_all_regex(input, r"{(?<= )\w+}")
  n_devices <- length(input)

  # visited <- new.env(parent = emptyenv(), size = n_devices)
  queue <- vector("list", n_devices)
  queue_last <- queue_first <- 1L
  queue[[queue_last]] <- list(device = first_key, dac = FALSE, fft = FALSE)

  path_count <- 0L

  while (queue_first <= length(queue)) {
    state <- queue[[queue_first]]
    device <- state$device
    pos <- which(devices == device)

    if (identical(device, last_key)) {
      if (state$dac && state$fft) {
        path_count <- path_count + 1L
      }
    } else {
      if (identical(device, "dac")) {
        dac <- TRUE
      } else {
        dac <- state$dac
      }

      if (identical(device, "fft")) {
        fft <- TRUE
      } else {
        fft <- state$fft
      }

      next_devices <- outputs[[pos]]
      next_states <- lapply(next_devices, \(x) list(device=x, dac=dac, fft=fft))

      next_in_queue <- queue_last + seq_along(next_devices)
      queue[next_in_queue] <- next_states
      queue_last <- queue_last + length(next_devices)
    }

    queue_first <- queue_first + 1L
  }

  path_count
}
