solve_day10_part2 <- function(input) {
  # everything after a { that is not a }
  goals <- stringi::stri_extract_first_regex(input, r"{(?<=\{)([^\}]+)}")
  goals <- strsplit(goals, ",")
  goals <- lapply(goals, as.numeric)

  # everything between parentheses ()
  button_list <- stringi::stri_extract_all_regex(input, r"{(?<=\()(.+?)(?=\))}")
  to_buttons <- \(button) as.numeric(unlist(strsplit(button, ","))) + 1
  button_list <- lapply(button_list, \(buttons) lapply(buttons, to_buttons))

  num_presses <- mapply(solve_min_presses, goals, button_list)

  sum(num_presses)
}

solve_min_presses <- function(goal, buttons) {
  n_pos     <- length(goal)
  n_buttons <- length(buttons)

  # Set constraints to find x where
  #   A %*% x = goal

  # Constraint matrix A: rows = positions, cols = buttons
  rows <- unlist(buttons)
  cols <- rep(seq_len(n_buttons), n_buttons)

  constraints_mat <- matrix(0L, nrow = n_pos, ncol = n_buttons)
  for (j in seq_along(buttons)) {
    idx <- buttons[[j]]
    constraints_mat[idx, j] <- 1
  }

  indices <- seq_along(buttons)
  constraints_mat[ unlist(buttons[indices]), indices]

  # Comparison direction of constraint
  constraints_dir <- rep("=", n_pos)

  # Coefficients of objective funtion, all 1
  # That is, all distances are 1 step
  objective <- rep(1, n_buttons)

  result <- lpSolve::lp(
    direction    = "min",
    objective.in = objective,
    const.mat    = constraints_mat,
    const.dir    = constraints_dir,
    const.rhs    = goal,
    all.int      = TRUE
  )

  result$objval
}
