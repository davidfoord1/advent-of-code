#' Find the total of valid calibrations
#'
#' Extract the target number as the first number on each line. Check if the
#' following sequence can total the target number, applying operators `+` or `*`
#' left-to-right in pairs. Sum the target numbers of all valid lines.
#'
#' @param input
#' Character vector of input lines with a target number followed by a sequence
#' of numbers to test against the target.
#'
#' @return
#' numeric(1) The total of targets that can be met
solve_day7_part1 <- function(input) {
  lines <- strsplit(input, ": | ")

  targets <- vapply(lines, \(line) line[[1]], character(1))
  targets <- as.numeric(targets)

  nums <- lapply(lines, \(line) line[-1])

  possible <- Map(memo_is_possible, targets, nums)

  sum(targets[as.logical(possible)])
}

#' Check if a sequence of numbers can meet a target
#'
#' Uses recursion to follow branches of applying one of a choice of operators
#' to sequential pairs of numbers. If at any point we exceed the target number,
#' returns FALSE. Once we have worked through the sequence to get a single
#' number. Return whether that number is the target number.
#'
#' The `operator`s used are `+` and `*`
#'
#' @param target
#' numeric(1) Target to match.
#' @param nums
#' Sequence of numbers to operate on.
#' @param operator
#' Function which takes 2 arguments to apply to the first 2 numbers in `nums`
#' ie the next operator to try. When NULL (default), the function just calls
#' itself with each operator.
#'
#' @return
#' logical(1) Whether the target can be met with the given sequence of numbers.
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