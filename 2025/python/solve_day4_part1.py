def solve_day4_part1(text):
    nrows = len(text)
    ncols = len(text[0])
    liftable_count = 0

    for row in range(nrows):
        for col in range(ncols):
            if is_liftable(text, row, col, nrows, ncols):
                liftable_count += 1

    return(liftable_count)

def is_liftable(grid, row, col, nrows, ncols):
    char = grid[row][col]
    roll_count = 0

    if char != "@":
        return(False)

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
                roll_count += 1

    return(roll_count < 4)
