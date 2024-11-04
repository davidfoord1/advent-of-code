add_year_heading <- function(year, con) {
  writeLines(
    c(
      "---",
      sprintf("  title: \"%s\"", year),
      "---",
      "",
      "```{r r-helpers}",
      "source(\"../utils/aoc_utils.R\")",
      "```",
      ""
    ),
    con
  )
}