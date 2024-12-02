# Set up ------------------------------------------------------------------

# Load lang_configs and pre-render functions
pre_render_files <- dir("pre-render", full.names = TRUE)
invisible(lapply(pre_render_files, source))

# Get the list of years
years <- dir(pattern = "^\\d{4}$")

# Page per year -----------------------------------------------------------

file_info_by_year <- vector("list", length(years))

for (year in years) {
  # Build list of files by language
  file_info_by_lang <- lapply(lang_configs,
                              get_file_info,
                              year)

  year_file_info <- do.call(rbind, file_info_by_lang)

  if (is.null(year_file_info)) next

  year_file_info[["year"]] <- year

  file_info_by_year <- append(file_info_by_year, list(year_file_info))

  output_file <- file.path(year, "nav.qmd")

  con <- file(output_file, open = "w")

  add_days_navigation(year_file_info, con)

  create_days_files(year_file_info)

  close(con)
}

# Index page --------------------------------------------------------------

# filter list in case a year folder exists but no scripts
has_file_info <- vapply(file_info_by_year, is.data.frame, logical(1))
file_info_by_year <- file_info_by_year[has_file_info]

all_file_info <- do.call(rbind, file_info_by_year)

if (length(all_file_info) <= 0) {
  stop("No solution files found.")
}

generate_index_page(all_file_info)
