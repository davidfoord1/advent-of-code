solve_day11_part2 <- function(input) {
  devices <- stringi::stri_extract_all_regex(input, r"{.+?(?=:)}")
  outputs <- stringi::stri_extract_all_regex(input, r"{(?<= )\w+}")
  n_devices <- length(input)

  prod(
    count_paths("svr", "fft"),
    count_paths("fft", "dac"),
    count_paths("dac", "out")
  )
}

count_paths <- function(first, last, device_list = devices, output_list = outputs) {
  if (first == last) return(1L)
  if (first == "out") return(0L)

  pos <- which(device_list == first)
  next_devices <- output_list[[pos]]

  dfs <- vapply(
    next_devices,
    \(next_device) count_paths(next_device, last),
    integer(1)
  )

  sum(dfs)
}

# the golden ticket
count_paths <- memoise::memoise(count_paths)
