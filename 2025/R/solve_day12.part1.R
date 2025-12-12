solve_day12_part1(input) {
  # Parsing ----
  pres_start <- which(substr(input, 2L, 2L) == ":") + 1L
  empty <- which(substr(input, 1L, 1L) == "")

  n_lines <- length(input)
  n_empty <- length(empty)
  n_presents <- length(pres_start)

  presents <- vector("list", n_presents)
  # present_lookup <- c("#" = TRUE, "." = FALSE)
  for (i in seq_len(n_presents)) {
    start <- pres_start[[i]]
    present <- input[start:(start + 2L)]
    present <- t(simplify2array(strsplit(present, "")))
    present[present == "#"] <- 1L
    present[present == "."] <- 0L
    storage.mode(present) <- "integer"
    presents[[i]] <- present
  }

  split <- empty[n_empty]
  trees <- input[(split + 1):n_lines]

  base_cols <- stringi::stri_extract_first_regex(trees, r"{\d+(?=x)}")
  base_cols <- as.integer(base_cols)

  base_rows <- stringi::stri_extract_first_regex(trees, r"{(?<=x)\d+(?=:)}")
  base_rows <- as.integer(base_rows)


  pres_counts <- stringi::stri_extract_all_regex(trees, r"{(?<= )\d+}")
  pres_counts <- lapply(pres_counts, as.integer)

  # Now what ----

  # for tree in trees
  #  for pres in pres counts
  #      try placing in every possible position?

  # can we shortcut with not enough space?
  base_areas <- base_cols * base_rows

  pres_areas <- vapply(presents, sum, integer(1))
  min_areas <- lapply(pres_counts, \(x) x * pres_areas)

  # plus a little more space is probably enough
  min_areas <- vapply(min_areas, sum, numeric(1)) * 1.1

  # uhh, ahahahaha
  sum(min_areas < base_areas)
}
