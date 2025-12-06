solve_day6_part2 <- function(input) {
  worksheet <- strsplit(input, "")
  nrows <- length(worksheet)

  nums <- worksheet[1:(nrows-1)]
  nums <- as.data.frame(nums)

  ops_row <- worksheet[[nrows]]
  starts <- which(ops_row != " ")
  ends <- c(starts[-1] - 2, NROW(nums))

  nums <- as.numeric(apply(nums, 1, \(x) paste0(x, collapse = "")))
  groups <- Map(\(start, end) nums[start:end], starts, ends)

  ops <- ops_row[starts]
  op_map <- list("+" = sum, "*" = prod)
  op_fns <- op_map[ops]

  ans <- mapply(\(fn, group) fn(group), op_fns, groups)

  sum(ans)
}
