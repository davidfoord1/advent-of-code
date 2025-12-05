def solve_day5_part2(text):
    split_at = text.index("\n")

    ranges = text[:split_at]
    ranges = [line.split("-") for line in ranges]
    ranges = [[int(range[0]), int(range[1].rstrip())] for range in ranges]

    ranges = sorted(ranges, key = lambda range: range[0])

    new_ranges = [ranges[0]]
    prev_ptr = 0

    for try_range in range(1, len(ranges)):
        try_lower, try_upper = ranges[try_range]
        prev_lower, prev_upper = new_ranges[prev_ptr]

        extends_prev = try_lower <= prev_upper and try_upper >= prev_upper

        if extends_prev:
            new_ranges[prev_ptr] = [prev_lower, try_upper]
            continue

        within_prev = try_lower >= prev_lower and try_upper <= prev_upper

        if not within_prev:
            new_ranges += [[try_lower, try_upper]]
            prev_ptr += 1

    id_count = 0

    for new_range in new_ranges:
        id_count += new_range[1] - new_range[0] + 1

    return(id_count)
