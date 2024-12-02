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
      text_header
    ),
    con
  )
}