solve_day6_part2 <- function(input) {
  worksheet <- strsplit(input, "")

  nums <- worksheet[1:(length(worksheet)-1)]
  nums <- as.data.frame(nums)

  ops_row <- worksheet[[length(worksheet)]]
  starts <- which(ops_row != " ")
  ends <- c(starts[-1] - 2, NROW(nums))

  nums <- as.numeric(apply(nums, 1, paste0, collapse = ""))
  nums <- Map(\(start, end) nums[start:end], starts, ends)

  ops <- ops_row[starts]
  ops <- ifelse(ops == "+", list(sum), list(prod))

  ans <- Map(\(fn, vec) fn(vec), ops, nums)
  ans <- unlist(ans)

  sum(ans)
}
