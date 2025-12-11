solve_day10_part1 <- function(input) {
  # everything after a [ that is not a ]
  goals <- stringi::stri_extract_first_regex(input, r"{(?<=\[)([^\]]+)}")
  goals <- stringi::stri_split_boundaries(goals, type='character')

  goal_lookup <- c("." = FALSE, "#" = TRUE)
  goals <- lapply(goals, \(goal) goal_lookup[goal])

  # everything between parentheses ()
  button_list <- stringi::stri_extract_all_regex(input, r"{(?<=\()(.+?)(?=\))}")
  to_buttons <- \(button) as.integer(unlist(strsplit(button, ","))) + 1L
  button_list <- lapply(button_list, \(buttons) lapply(buttons, to_buttons))

  num_presses <- mapply(min_presses, goals, button_list)

  sum(num_presses)
}

min_presses <- function(goal, buttons) {
  start <- goal
  start[] <- FALSE # all off

  states <- list(start)
  presses  <- 0L

  n_buttons <- length(buttons)

  visited <- new.env(hash = TRUE, parent = emptyenv())
  key_start <- paste0(as.integer(start), collapse = "")
  visited[[key_start]] <- TRUE

  repeat {
    presses <- presses + 1L

    next_states <- list()
    idx <- 0L

    for (lights in states) {
      for (toggle in buttons) {
        next_lights <- lights
        next_lights[toggle] <- !next_lights[toggle]

        # Exit if at goal ---
        if (identical(next_lights, goal)) {
          return(presses)
        }

        key <- paste0(as.integer(next_lights), collapse = "")
        if (!exists(key, envir = visited, inherits = FALSE)) {
          visited[[key]] <- TRUE
          idx <- idx + 1L
          next_states[[idx]] <- next_lights
        }
      }
    }

    states <- next_states
  }
}
