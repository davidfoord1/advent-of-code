def solve_day7_part1(text):
    nrows = len(text)
    ncols = len(text[0])

    source = text[0].index("S")
    beams = [[0] * ncols for _ in range(nrows)]
    beams[0][source] = 1

    split_count = 0

    for row in range(nrows-1):
        next_row = row + 1
        beam_cols = [i for i, val in enumerate(beams[row]) if val > 0]

        for col in beam_cols:
            if text[next_row][col] == "^":
                beams[next_row][col+1] += 1
                beams[next_row][col-1] += 1
                split_count += 1
                continue

            if beams[next_row][col] == 0:
                beams[next_row][col] = 1

    return(split_count)
