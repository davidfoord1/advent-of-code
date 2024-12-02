#' @param file_info file_info for one year
create_days_files <- function(file_info) {
  year <- unique(file_info[["year"]])
  days <- sort(unique(file_info[["day"]]))
  year_qmd_dir <- year

  for (day in days) {
    day_file <- file.path(year_qmd_dir,
                          paste0(day, ".qmd"))

    day_con <- file(day_file, open = "w")

    # YAML header ----
    writeLines(
      c("---",
        sprintf("  title: \"%s\"", year),
        "---"),
      day_con
    )

    # In year navigation ----

    writeLines("<script src=\"../scripts/day-nav.js\"></script>", day_con)

    writeLines("{{< include nav.qmd >}}", day_con)

    # Day content ----

    ## Load utils functions ----
    writeLines(
      c(
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
        ""
      ),
      day_con
    )

    ## Written overview of day ----
    md <- sprintf("%s/markdown/day%s.md", year, day)
    include_md <- sprintf("\n{{< include markdown/day%s.md >}}\n", day)

    if (file.exists(md)) {
      writeLines(include_md, day_con)
    }

    ## Language content ----

    day_languages <- unique(file_info[["language"]][file_info[["day"]] == day])

    writeLines("::: {.panel-tabset .language-set}\n", day_con)

    for (language in day_languages) {
      lang_info <- file_info[file_info[["day"]] == day & file_info[["language"]] == language, ]
      lang_config <- lang_configs[[language]]

      writeLines(sprintf("## %s \n", lang_config[["name"]]), day_con)

      # Get parts for this day and language
      parts <- sort(unique(lang_info[["part"]]))

      # Add content for each part
      invisible(lapply(
        parts,
        FUN = add_part_content,
        day,
        lang_info,
        lang_config,
        day_con
      ))
    }

    writeLines(":::\n", day_con)
  }

  close(day_con)
}