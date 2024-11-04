generate_index_page <- function(all_file_info) {
  # Assumes every save script is a completed star
  stars_count <- aggregate(part ~ year + language + day,
                           data = all_file_info,
                           NROW)

  names(stars_count)[names(stars_count) == "part"] <- "parts"

  days <- 1:25
  languages <- unique(all_file_info[["language"]])
  years <- sort(unique(all_file_info[["year"]]))

  grid <- expand.grid(
    day = days,
    language = languages,
    year = years,
    stringsAsFactors = FALSE
  )

  stars_count <- merge(
    grid,
    stars_count,
    by = c("year", "language", "day"),
    all.x = TRUE,
    sort = FALSE
  )

  stars_count[["parts"]][is.na(stars_count[["parts"]])] <- 0

  stars_count <- stars_count |>
    transform(parts = vapply(
      parts,
      \(x) paste(rep("<span class=\"star\">\\*</span>", x), collapse = ""),
      character(1)
    ))

  stars_count[["stars"]] <- ifelse(
    nchar(stars_count[["parts"]]) > 0,
    paste(stars_count[["language"]], stars_count[["parts"]]),
    ""
  )

  stars_count[["year_language"]] <- paste(stars_count[["year"]], stars_count[["language"]], sep = "_")


  stars_wide <- reshape(
    stars_count,
    idvar = "day",
    timevar = "year_language",
    direction = "wide",
    drop = c("year", "language")
  )

  stars_wide <- sort_by(stars_wide, stars_wide[["day"]])

  for (year in years) {
    stars_wide[[year]] <- ""

    for (lang in languages) {
      col_name <- sprintf("stars.%s_%s", year, lang)

      stars_wide[[year]] <- paste(
        stars_wide[[year]],
        stars_wide[[col_name]]
      )
    }
  }
}