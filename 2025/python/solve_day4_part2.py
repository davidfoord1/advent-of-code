def solve_day4_part2(text):
    grid = [list(row) for row in text]
    nrows = len(grid)
    ncols = len(grid[0])
    removed_count = 0
    removing_rolls = True

    while removing_rolls:
        removing_rolls = False
        for row in range(nrows):
            for col in range(ncols):
                if grid[row][col] != "@":
                    continue

                neighbours = 0
                for row_adj in range(-1, 2):
                    search_row = row + row_adj

                    if search_row < 0 or search_row >= nrows:
                        continue

                    for col_adj in range(-1, 2):
                        search_col = col + col_adj
                        if search_col < 0 or search_col >= ncols:
                            continue
                        if row == search_row and col == search_col:
                            continue

                        if grid[search_row][search_col] == "@":
                            neighbours += 1

                if neighbours < 4:
                    removing_rolls = True
                    removed_count += 1
                    grid[row][col] = "."

    return(removed_count)
