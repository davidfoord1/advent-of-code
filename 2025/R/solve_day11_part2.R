solve_day11_part2 <- function(input) {
  lookup <- stringi::stri_extract_all_regex(input, r"{(?<= )\w+}")
  names(lookup) <- stringi::stri_extract_all_regex(input, r"{.+?(?=:)}")
  n_devices <- length(input)

  prod(
    count_paths("svr", "fft", lookup),
    count_paths("fft", "dac", lookup),
    count_paths("dac", "out", lookup)
  )
}

count_paths <- function(from, to, lookup) {
  if (from == to) return(1L)
  if (from == "out") return(0L)

  next_nodes <- lookup[[from]]

  dfs <- vapply(
    next_nodes,
    \(node) count_paths(node, to, lookup),
    integer(1)
  )

  sum(dfs)
}

# the golden ticket
count_paths <- memoise::memoise(count_paths)
