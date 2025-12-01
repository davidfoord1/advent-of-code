def solve_day1_part2(text):
    zero_counter = 0
    dial_size = 100
    pos = 50

    for line in text:
        if line[0] == "R":
            step = 1
        else:
            step = -1

        amount = int(line[1:])

        for _ in range(amount):
            pos = (pos + step) % dial_size
            if pos == 0:
                zero_counter += 1

    return(zero_counter)
