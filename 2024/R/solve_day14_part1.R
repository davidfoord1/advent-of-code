solve_day14_part1 <- function(input) {
  bots <- stringi::stri_extract_all_regex(input, "-?\\d+")
  width <- 101L
  height <- 103L
  seconds <- 100L

  bots <- purrr::list_transpose(bots)
  bots <- lapply(bots, as.integer)
  names(bots) <- c("x_pos", "y_pos", "x_vel", "y_vel")

  # 0-based index to 1-based (not sorry)
  bots[["x_pos"]] <- bots[["x_pos"]] + 1L
  bots[["y_pos"]] <- bots[["y_pos"]] + 1L

  for (sec in seq_len(seconds)) {
    bots <- move_bots(bots, width, height)
  }

  bots <- assign_quadrant(bots, width, height)

  bots_per_quad <- table(bots[["quad"]])

  Reduce(`*`, bots_per_quad)

  # 220086360 too high
  # 216027840
}

#' Title
#'
#' @param bots
#'
#' @return
move_bots <- function(bots, width, height) {
  bots <- transform(
    bots,
    x_pos = x_pos + x_vel,
    y_pos = y_pos + y_vel
  )

  bots <- transform(
    bots,
    x_pos = ifelse(x_pos < 1L, x_pos + width, x_pos),
    y_pos = ifelse(y_pos < 1L, y_pos + height, y_pos)
  )

  bots <- transform(
    bots,
    x_pos = ifelse(x_pos > width, x_pos - width, x_pos),
    y_pos = ifelse(y_pos > height, y_pos - height, y_pos)
  )

  bots
}

assign_quadrant <- function(bots, width, height) {
  x_half <- ceiling(width / 2L)
  y_half <- ceiling(height / 2L)

  bots[["quad"]] <- NA_integer_

  bots <- bots |>
    transform(quad = ifelse(x_pos < x_half & y_pos < y_half, 1L, quad)) |>
    transform(quad = ifelse(x_pos > x_half & y_pos < y_half, 2L, quad)) |>
    transform(quad = ifelse(x_pos < x_half & y_pos > y_half, 3L, quad)) |>
    transform(quad = ifelse(x_pos > x_half & y_pos > y_half, 4L, quad))

  bots
}

