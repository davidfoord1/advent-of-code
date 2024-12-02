add_days_navigation <- function(file_info, con) {
  year <- unique(file_info[["year"]])
  days <- sort(unique(file_info[["day"]]))
  year_qmd_dir <- year

  writeLines("<div class=\"day-nav\">", con)

  for (day in days) {
    writeLines(
      paste0("[Day ", day, "](../", year, "/", day, ".qmd)"),
      con
    )
  }

  writeLines("</div>", con)
}