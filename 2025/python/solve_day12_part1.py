import re
def solve_day12_part1(text):
    spaces = [i for i, v in enumerate(text) if v == "\n"]
    split = spaces[len(spaces)-1] + 1
    trees = text[split:]

    widths = [int(line[:2]) for line in trees]
    heights = [int(line[3:5]) for line in trees]
    areas = [w * h for w, h in zip(widths, heights)]

    counts = [re.findall("(?<= )\d+", line) for line in trees]
    counts = [[int(x) for x in line] for line in counts]
    counts = [sum(line) for line in counts]

    valid = [area >= (9 * count) for area, count in zip(areas, counts)]

    sum(valid)
