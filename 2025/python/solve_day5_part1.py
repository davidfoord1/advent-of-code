def solve_day5_part1(text):
    split_at = text.index("\n")

    ranges = text[:split_at]
    ranges = [line.split("-") for line in ranges]
    ranges = [[int(range[0]), int(range[1].rstrip())] for range in ranges]

    ids = text[(split_at+1):]
    ids = [int(id) for id in ids]

    id_count = 0

    for id in ids:
        for range in ranges:
            if id >= range[0] and id <= range[1]:
                id_count += 1
                break

    return(id_count)
