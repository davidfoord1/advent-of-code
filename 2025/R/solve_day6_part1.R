solve_day6_part1 <- function(input) {
    worksheet = read.table(text = input)

    nums <- worksheet[1:(NROW(worksheet)-1), ]

    for (col in seq_along(nums)) {
        nums[[col]] <- as.numeric(nums[[col]])
    }

    ops <- worksheet[NROW(worksheet), ]
    ans <- numeric(length(worksheet))

    for (i in seq_along(worksheet)) {
        operation = switch(
            ops[, i],
            "*" = `*`,
            "+" = `+`,
        )

        ans[i] = Reduce(operation, nums[, i])
    }

  sum(ans)
}
