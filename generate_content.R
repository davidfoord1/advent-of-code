# Load lang_configs and pre-render functions
pre_render_files <- dir("pre-render", full.names = TRUE)
invisible(lapply(pre_render_files, source))

# Get the list of years
years <- dir(pattern = "^\\d{4}$")

for (year in years) {
  # Build list of files by language
  file_info_list <- lapply(lang_configs,
                           get_file_info,
                           year)

  if (length(file_info_list) <= 0) {
    message(sprintf("No scripts found for year %s.", year))
  }


  file_info <- do.call(rbind, file_info_list)

  days <- sort(unique(file_info[["day"]]))

  output_file <- file.path(year, sprintf("generated_content_%s.qmd", year))

  con <- file(output_file, open = "w")

  close(con)
}

