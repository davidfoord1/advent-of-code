solve_day3_part1 <- function(input) {
  muls <- stringi::stri_extract_all_regex(input,
                                          "(?<=mul\\()[0-9]+,[0-9]+(?=\\))")

  muls <- unlist(muls)
  muls <- strsplit(muls, ",")
  muls <- as.data.frame(t(as.data.frame(muls)))

  sum( as.numeric(muls[ ,1]) * as.numeric(muls[, 2]) )
}
