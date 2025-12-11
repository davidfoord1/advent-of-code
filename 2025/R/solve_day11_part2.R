solve_day11_part2 <- function(input) {
  devices <- stringi::stri_extract_all_regex(input, r"{.+?(?=:)}")
  outputs <- stringi::stri_extract_all_regex(input, r"{(?<= )\w+}")
  n_devices <- length(input)

  prod(
    count_paths("svr", "fft", devices, outputs),
    count_paths("fft", "dac", devices, outputs),
    count_paths("dac", "out", devices, outputs)
  )
}

count_paths <- function(from, to, devices, outputs) {
  if (from == to) return(1L)
  if (from == "out") return(0L)

  pos <- which(devices == from)
  next_devices <- outputs[[pos]]

  dfs <- vapply(
    next_devices,
    \(next_device) count_paths(next_device, to, devices, outputs),
    integer(1)
  )

  sum(dfs)
}

# the golden ticket
count_paths <- memoise::memoise(count_paths)
