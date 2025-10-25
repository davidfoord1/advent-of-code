solve_day13_part2 <- function(input) {
  nums <- stringi::stri_extract_all_regex(input, "\\d+")
  nums <- na.omit(as.numeric(unlist(nums)))
  machines <- matrix(nums, ncol = 6, byrow = TRUE)
  machines <- as.data.frame(machines)
  names(machines) <- c("XA", "YA", "XB", "YB", "XP", "YP")


  machines <- machines |>
    transform(XP = XP + 10000000000000,
              YP = YP + 10000000000000) |>
    transform(
      A = (XP * YB - YP * XB) / (XA * YB - YA * XB),
      B = (XA * YP - YA * XP) / (XA * YB - YA * XB)
    ) |>
    transform(is_valid = A == round(A) & B == round(B)) |>
    transform(cost = ifelse(is_valid, 3L * A + B, 0L))

  sum(machines[["cost"]])
}
