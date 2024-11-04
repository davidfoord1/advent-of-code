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

  if (length(file_info_list) > 0) {
    file_info <- do.call(rbind, file_info_list)

    days <- sort(unique(file_info[["day"]]))
  }

}

