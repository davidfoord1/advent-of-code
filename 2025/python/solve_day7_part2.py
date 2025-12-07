def solve_day7_part2(text):
    nrows = len(text)
    ncols = len(text[0])

    source = text[0].index("S")
    beams = [[0] * ncols for _ in range(nrows)]
    beams[0][source] = 1

    for row in range(nrows-1):
        next_row = row + 1
        beam_cols = [i for i, val in enumerate(beams[row]) if val > 0]

        for col in beam_cols:
            n_beams = beams[row][col]
            if text[next_row][col] == "^":
                beams[next_row][col+1] += n_beams
                beams[next_row][col-1] += n_beams
                continue

            beams[next_row][col] += n_beams

    return(sum(beams[nrows-1]))
