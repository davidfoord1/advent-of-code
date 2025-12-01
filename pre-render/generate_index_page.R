generate_index_page <- function(all_file_info) {
  # Assumes every save script is a completed star
  stars_count <- aggregate(
    part ~ year + language + day,
    data = all_file_info,
    NROW
  )

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

  write_stars <- function(x) {
    paste(rep("<span class=\"star\">\\*</span>", x), collapse = "")
  }

  stars_count <- stars_count |>
    transform(
      parts = vapply(
        parts,
        write_stars,
        character(1)
      )
    )

  stars_count[["stars"]] <- ifelse(
    nchar(stars_count[["parts"]]) > 0,
    paste(stars_count[["language"]], stars_count[["parts"]]),
    ""
  )

  stars_count[["year_language"]] <- paste(
    stars_count[["year"]],
    stars_count[["language"]],
    sep = "_"
  )

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

    # add year-day navigation links
    # can find elements as .table a
    stars_wide[[year]] <- ifelse(
      grepl("star", stars_wide[[year]]),
      stars_wide[[year]] <- paste0(
        "[",
        stars_wide[[year]],
        "](",
        year,
        "/",
        stars_wide[["day"]],
        ".qmd)"
      ),
      ""
    )
  }

  stars_wide <- stars_wide[names(stars_wide) %in% c("day", years)]

  separator_row <- rep("---", length(names(stars_wide)))

  table_lines <- c(
    paste(names(stars_wide), collapse = " | "),
    paste(separator_row, collapse = " | ")
  )

  for (row in seq_len(NROW(stars_wide))) {
    row_values <- stars_wide[row, ]
    table_lines <- c(table_lines, paste(row_values, collapse = " | "))
  }

  table_lines <- paste("|", table_lines, "|")

  index_file <- "index.qmd"
  index_con <- file(index_file, open = "w")

  writeLines("---", index_con)
  writeLines("title: David's Advent of Code Solutions", index_con)
  writeLines("---", index_con)

  intro_text <- readLines("intro.md")
  writeLines(intro_text, index_con)

  # Table heading
  writeLines("", index_con)
  writeLines("### Star collection", index_con)
  writeLines("", index_con)

  writeLines(table_lines, index_con)

  close(index_con)
}
