get_file_info <- function(lang_config, year) {
  pattern <- sprintf("^solve_day\\d+_part\\d+\\.%s$",
                     lang_config[["extension"]])

  folder <- file.path(year, lang_config[["folder"]])

  if (!dir.exists(folder)) return(NULL)

  files <- dir(folder, pattern, full.names = TRUE)

  if (length(files) <= 0) return(NULL)

  file_info <- data.frame(
    file            = files,
    language        = lang_config[["name"]]
  )

  filenames <- basename(files)

  days <- as.integer(stringr::str_extract(
    filenames,
    "(?<=_day)\\d+"
  ))


  parts <- as.integer(stringr::str_extract(
    filenames,
    "(?<=_part)\\d+"
  ))

  data.frame(
    file = files,
    filename = filenames,
    language = lang_config[["name"]],
    day = days,
    part = parts
  )
}