add_year_heading <- function(year, con) {
  writeLines(
    c(
      "---",
      sprintf("  title: \"%s\"", year),
      "---",
      "",
      "```{r r-helpers}",
      "# import R helpers",
      "source(\"../utils/aoc_utils.R\")",
      "```",
      "```{python python-helpers}",
      "# import Python helpers",
      "with open(\"../utils/aoc_utils.py\") as file:",
      "    exec(file.read())",
      "```",
      ""
    ),
    con
  )
}