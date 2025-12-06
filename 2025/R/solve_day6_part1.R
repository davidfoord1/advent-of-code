solve_day6_part1 <- function(input) {
    worksheet = read.table(text = input)

    nums <- worksheet[1:(NROW(worksheet)-1), ]
    nums <- apply(nums,2, as.numeric, simplify = FALSE)

    ops <- worksheet[NROW(worksheet), ]
    ops <- ifelse(ops == "+", list(sum), list(prod))

    ans <- Map(\(fn, vec) fn(vec), ops, nums)
    ans <- unlist(ans)

    sum(ans)
}
