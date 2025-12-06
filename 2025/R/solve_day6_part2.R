solve_day6_part2 <- function(input) {
  worksheet <- strsplit(input, "")

  nums <- worksheet[1:(length(worksheet)-1)]
  nums <- as.data.frame(nums)

  len <- NROW(nums[[1]])
  boundaries <- numeric(len)
  boundary_ptr <- 1L

  for (i in seq_len(len)) {
    if (all(nums[i, ] == " ")) {
      boundaries[boundary_ptr] <- i
      boundary_ptr <- boundary_ptr + 1
    }
  }

  boundaries <- boundaries[!boundaries == 0]
  boundaries <- c(rev(boundaries), 1)

  problem_nums <- vector("list", length(boundaries))

  start <- len
  for (problem in seq_along(boundaries)) {
    end <- boundaries[[problem]]

    curr_num_len <- start - end + 1
    curr_nums <- numeric(curr_num_len)

    for (i in seq_len(curr_num_len)) {
      curr_num <- start - i + 1
      curr_nums[i] <- as.numeric(paste0(nums[curr_num, ], collapse = ""))
    }

    problem_nums[[problem]] <- curr_nums
    start <- boundaries[[problem]] - 1
  }

  problem_nums <- lapply(problem_nums, na.omit)
  problem_nums <- lapply(problem_nums, unlist)
  problem_nums <- rev(problem_nums)

  ops_sheet <- read.table(text = input)
  ops <- ops_sheet[NROW(ops_sheet), ]
  ans <- numeric(length(ops_sheet))

  for (i in seq_along(ops_sheet)) {
      operation = switch(
          ops[, i],
          "*" = `*`,
          "+" = `+`,
      )

      ans[i] = Reduce(operation, problem_nums[[i]])
  }

  sum(ans)
}
