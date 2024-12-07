solve_day7_part1 <- function(input) {
  lines <- strsplit(input, ": | ")

  targets <- vapply(lines, \(line) line[[1]], character(1))
  targets <- as.numeric(targets)

  nums <- lapply(lines, \(line) line[-1])

  possible <- Map(memo_is_possible, targets, nums)

  sum(targets[as.logical(possible)])
}

is_possible <- function(target, nums, operator = NULL) {
  nums <- as.numeric(nums)

  if (length(nums) == 1) {
    return(nums == target)
  }

  if (!is.null(operator)) {
    new_num <- as.numeric(operator(nums[[1]], nums[[2]]))

    # prune
    if(new_num > target) {
      return(FALSE)
    }

    nums <- c(new_num, nums[-c(1, 2)])
  }

  # try each operator
  is_possible(target, nums, `+`) ||
    is_possible(target, nums, `*`)
}

#' Cache results as you go to save recalculations for the same combinations
memo_is_possible <- memoise::memoise(is_possible)