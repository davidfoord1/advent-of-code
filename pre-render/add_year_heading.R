add_year_heading <- function(year, con) {
  text_header <- ""

  if (file.exists(paste0(year, "/markdown/text_header.md"))) {
    text_header <- "{{< include markdown/text_header.md >}}"
  }

  writeLines(
    c(
      "---",
      sprintf("  title: \"%s\"", year),
      "---",
      "",
      "```{r r-helpers}",
      "#| echo: false",
      "# import R helpers",
      "source(\"../utils/aoc_utils.R\")",
      "```",
      "```{python python-helpers}",
      "#| echo: false",
      "# import Python helpers",
      "with open(\"../utils/aoc_utils.py\") as file:",
      "    exec(file.read())",
      "```",
      "",
      text_header
    ),
    con
  )
}