add_days_content <- function(file_info, con) {
  days <- sort(unique(file_info[["day"]]))

  # Generate the markdown content
  writeLines("::: {.panel-tabset}\n", con)

  for (day in days) {
    writeLines(sprintf("## Day %d\n", day), con)

    # Add written overview of the day
    md <- sprintf("%s/markdown/day%s.md", unique(file_info[["year"]]), day)
    include_md <- sprintf("\n{{< include markdown/day%s.md >}}\n", day)

    if (file.exists(md)) {
      writeLines(include_md, con)
    }

    # Get the languages available for this day
    day_languages <- unique(file_info[["language"]][file_info[["day"]] == day])

    writeLines("::: {.panel-tabset .language-set}\n", con)

    for (language in day_languages) {
      lang_info <- file_info[file_info[["day"]] == day & file_info[["language"]] == language, ]
      lang_config <- lang_configs[[language]]

      writeLines(sprintf("## %s \n", lang_config[["name"]]), con)

      # Get parts for this day and language
      parts <- sort(unique(lang_info[["part"]]))

      # Add content for each part
      invisible(lapply(
        parts,
        FUN = add_part_content,
        day,
        lang_info,
        lang_config,
        con
      ))
    }

    writeLines(":::\n", con)
  }

  writeLines(":::\n", con)
}

add_part_content <- function(part, day, lang_info, lang_config, con) {
  writeLines(sprintf("### --- %s Day %s Part %d ---\n",
                     lang_config[["name"]],
                     day,
                     part),
             con)



  # Include the code (where it is shown but not run)
  code_filepath <- lang_info[["file"]][lang_info[["part"]] == part]

  # Adjust the code_filepath to be relative to the content.qmd file
  code_filepath_relative <- file.path(lang_config[["folder"]], basename(code_filepath))

  code_include <- sprintf(
    "```%s\n{{< include %s >}}\n```\n",
    lang_config[["code_chunk_lang"]], code_filepath_relative
  )

  writeLines(code_include, con)

  # Include the code block to run it
  chunk_name <- sprintf("%s_day%d_part%d", tolower(lang_config[["name"]]), day, part)
  aoc_run_call <- sprintf(lang_config[["aoc_run_template"]], day, part)

  # exec code block
  exec_code_block <- paste(
    # Chunk header
    sprintf("```{%s}", lang_config[["code_chunk_lang"]]),
    sprintf("#| label: %s", chunk_name),
    "#| code-fold: true",
    "#| code-summary: \"Run\"",

    # Load solve function
    sprintf("aoc_source(day = %d, part = %d)\n", day, part),
    # Read input
    sprintf("input = aoc_read(day = %d)\n", day),
    # Run solve function
    sprintf(lang_config[["aoc_run_template"]], day, part),

    # Close chunk
    "```",

    sep = "\n"
  )

  writeLines(exec_code_block, con)
}
