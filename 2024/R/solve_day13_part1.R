solve_day13_part1 <- function(input) {
  nums <- stringi::stri_extract_all_regex(input, "\\d+")
  nums <- na.omit(as.numeric(unlist(nums)))
  machines <- matrix(nums, ncol = 6, byrow = TRUE)
  machines <- as.data.frame(machines)
  names(machines) <- c("XA", "YA", "XB", "YB", "XP", "YP")


  machines <- machines |>
    transform(
      A = (XP * YB - YP * XB) / (XA * YB - YA * XB),
      B = (XA * YP - YA * XP) / (XA * YB - YA * XB)
    ) |>
    transform(is_valid = A == round(A) & B == round(B)) |>
    transform(cost = ifelse(is_valid, 3L * A + B, 0L))

  sum(machines[["cost"]])
  #
  # Button A: X+94, Y+34
  # Button B: X+22, Y+67
  # Prize: X=8400, Y=5400

  # (1) a * XA + b * XB = XP
  # (2) a * YA + b * YB = YP

  # a from (2)
  # a = (YP - b * YB) / YA
  # a = (YP / YA) - b * (YB / YA)

  # a into (1)
  # (YP / YA) - b * (YB / YA) * XA + b * XB = XP
  # b * -(YB / YA) * XA + b * XB = XP - (YP / YA)
  # b * (-(YB / YA) * XA + XB)  = XP - (YP / YA)
  # b = (XP - (YP / YA)) / (-(YB / YA) * XA + XB)
  machines <- machines |>
    transform(b = (XP - (YP / YA)) / (-(YB / YA) * XA + XB))

  # no more than 100 times to win
  machines <- machines[machines[["b"]] <= 100, ]

  machines <- machines |>
    # we need integers and have created a fraction
    # so try both the floor and ceiling
    transform(b_low = floor(b),
              b_high = ceiling(b)) |>
    # b into (1)
    transform(a1 = (YP - b_low * YB) / YA,
              a2 = (YP - b_high * YB) / YA) |>
    # is a valid win if a is an integer
    # transform(
    #   valid1 = a1 %% 1 == 0 & a1 * XA + b_low * XB == XP,
    #   valid2 = a2 %% 1 == 0 & a2 * XA + b_high * XB == XP
    # ) |>

    transform(
      valid1 = a1 %% 1 == 0 & a1 >= 0,
      valid2 = a2 %% 1 == 0 & a2 >= 0
    ) |>

    transform(cost1 = ifelse(valid1, 3 * a1 + b_low, 0L),
              cost2 = ifelse(valid2, 3 * a2 + b_high, 0L)) |>

    transform(cost = cost1 + cost2)

    machines <- machines[machines[["a1"]] <= 100L & machines[["a2"]] <= 100L, ]

  # machines <- machines[abs(machines[["cost"]]) > 0, ]

  sum(machines[["cost"]])
}
