solve_day6_part2 <- function(input) {
  worksheet <- strsplit(input, "")
  nrows <- length(worksheet)

  nums <- worksheet[1:(nrows-1)]
  nums <- as.data.frame(nums)

  ops_row <- worksheet[[nrows]]
  starts <- which(ops_row != " ")
  ends <- c(starts[-1] - 2, NROW(nums))

  nums <- as.numeric(apply(nums, 1, paste0, collapse = ""))
  groups <- Map(\(start, end) nums[start:end], starts, ends)

  ops <- ops_row[starts]
  ops <- ifelse(ops == "+", list(sum), list(prod))

  ans <- mapply(\(fn, vec) fn(vec), ops, groups)

  sum(ans)
}
