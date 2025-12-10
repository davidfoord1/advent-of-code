solve_day10_part1 <- function(input) {
  # everything after a [ that is not a ]
  goals <- stringi::stri_extract_first_regex(input, r"{(?<=\[)([^\]]+)}")
  goals <- stringi::stri_split_boundaries(goals, type='character')

  goal_lookup <- c("." = FALSE, "#" = TRUE)
  goals <- lapply(goals, \(goal) goal_lookup[goal])

  # everything between parentheses ()
  button_list <- stringi::stri_extract_all_regex(input, r"{(?<=\()(.+?)(?=\))}")
  to_buttons <- \(button) as.numeric(unlist(strsplit(button, ","))) + 1
  button_list <- lapply(button_list, \(buttons) lapply(buttons, to_buttons))

  num_presses <- mapply(min_presses, goals, button_list)

  sum(num_presses)
}

min_presses <- function(goal, buttons) {
  start <- goal
  start[] <- FALSE

  n_buttons <- length(buttons)
  button_nums <- seq_len(n_buttons)

  queue <- vector("list", 10e4)
  queue_next <- queue_last <- 1L
  queue[[queue_next]] <- list("lights" = start, "presses" = 0)

  while(TRUE) {
    prev_state <- queue[[queue_next]]
    lights <- prev_state[["lights"]]
    presses <- prev_state[["presses"]] + 1

    next_states <- lapply(
      buttons,
      \(x) {
        next_lights <- lights
        next_lights[x] <- !next_lights[x]

        list("lights" = next_lights, presses = presses)
      }
    )

    for (state in next_states) {
      if (all(state[["lights"]] == goal)) {
        return(presses)
      }
    }

    queue_pos <- queue_last + seq_along(next_states)
    queue[queue_pos] <- next_states
    queue_last <- queue_last + length(next_states)

    # grow queue if reach 90% capacity
    list_len <- length(queue)
    if (queue_last > 0.9 * list_len) {
      tmp <- queue
      queue <- vector("list", 2*list_len)
      queue[seq_len(list_len)] <- tmp
    }

    queue_next <- queue_next + 1
  }
}
