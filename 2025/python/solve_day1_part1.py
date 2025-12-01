def solve_day1_part1(text):
    zero_counter = 0
    dial_size = 100
    pos = 50

    turns = [turn.replace("L", "-").replace("R", "") for turn in text]
    turns = [int(turn) for turn in turns]

    for turn in turns:
        pos = (pos + turn) % dial_size

        zero_counter += pos == 0

    return(zero_counter)
