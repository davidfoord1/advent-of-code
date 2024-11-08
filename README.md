## Advent of Code solutions

Source code for a site featuring R and Python solutions to Advent of Code
challenges.

### Steps to reproduce the site

1. For every day and part that has a script written, save your input files to
`<year>/input/day<number>.txt` like `2024/input/day1.txt`.

2. Run `quarto render` at the project root. The site will be rendered to the
`docs/` folder.

### Project Structure

All Quarto markdown documents are made by `generate_content.R` and the functions
it calls from `pre-render/`. The project directories are searched for solutions
by year, language, day and part. Solutions are then pieced together into one
`content.qmd` per year.

Aside from `utils/` scripts that are shared between years, years are largely
self-contained. Each year will have its own `input/` subdirectory, and
subdirectories for each language with solutions.

```
advent-of-code/
├── _quarto.yml                   # Quarto project definition     
├── generate_content.R            # Control script to create .qmd files
├── pre-render/                   # Functions used to create .qmd files
│   ├── language_configuration.R    # Config for languages e.g. R and Python
│   ├── get_file_info.R             # Search <year>/<lang>/ for scripts
│   ├── add_year_heading.R          # Content that's the same for all days
│   ├── add_days_content.R          # Add individual day and part content
│   └── generate_index_page.R       # Build index.qmd and star count table
├── docs/                         # Rendered content for GitHub pages
├── utils/                        
│   ├── aoc_utils.R                 # Utility funcs included by add_year_heading
│   └── aoc_utils.py                # Utility funcs included by add_year_heading
├── intro.md                     # Home page introduction text
├── index.qmd                    # Auto-generated Quarto home page
└── <year>/                      # Separate folders for each year
    ├── content.qmd/              # Auto-generated Quarto page per year
    ├── input/                      # Save inputs as text files here
    ├── markdown/                   # Year and day overview markdown files
    ├── R/                          # R lang solution functions
    └── python/                     # Python solution functions
```

#### Add a new solution (day/part)

Within the year directory, under the solution's language subdirectory, add a new
file `solve_day<number>_part<number>.<extension>` which hosts a function called
`solve_day<number>_part<number>`.

For example to add the first task of 2024, I created `2024/R/solve_day1_part1.R`
and function `solve_day1_part1()`.

#### Add a new language

There are utils functions for each language: 

- `aoc_source(day, part)` to load the functions needed for each part's solution.
- `aoc_read(day)` to import the input for the respective day.
- `aoc_run()` to run, time and print the results of a solution function.

1. A new language will need its own utils functions setup, or otherwise be
supported from within R. To make the functions available on each year's page,
import them from files in `utils/` in the script
`pre-render/add_year_heading.R`.

2. Fill out the details for the language in
`pre-render/language_configuration.R`.

3. In the file `pre-render/add_days_content.R` there is a function
`add_part_content()`. Ensure that it's variable `exec_code_block` supports
executing code in the new language, a long with executing the `aoc_run_template`
specified in the language config.

4. Start adding scripts for the new language in its own subdirectory within each
year.

#### Add a new year

1. Create a new folder with a 4-digit name for the year. Populate as above with
`input/` and solutions by language.

2. Add `- <year>/content.qmd` to the `navbar` in `_quarto.yml`.
