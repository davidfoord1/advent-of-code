solve_day6_part1 <- function(input) {
    worksheet = read.table(text = input)

    nums <- worksheet[1:(NROW(worksheet)-1), ]
    nums <- apply(nums, 2, as.numeric, simplify = FALSE)

    ops <- worksheet[NROW(worksheet), ]
    ops <- ifelse(ops == "+", list(sum), list(prod))

    ans <- mapply(\(fn, group) fn(group), ops, nums)

    sum(ans)
}
