# `folder` is the subdirectory for the language to be found in each year
# directory

# `extension` is the file extension to search for

# `code_chunk_lang` is Quarto code chunk supported language name

# `aoc_run_template` is a base::sprintf() supported string of the current
# required to run the solution function for a given day and part, i.e. it should
# have 2x %d
lang_configs <- list(

  R = list(
    name = "R",
    folder = "R",
    extension = "R",
    code_chunk_lang = "r",
    aoc_run_template = 'aoc_run(solve_day%d_part%d(input))'
  ),

  Python = list(
    name = "Python",
    folder = "python",
    extension = "py",
    code_chunk_lang = "python",
    aoc_run_template = 'result = aoc_run("solve_day%d_part%d(input)")'
  )

  # Add more languages here
)