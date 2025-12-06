from math import prod

def solve_day6_part2(text):
    text = [[*line.rstrip("\n")] for line in text]

    nrows = len(text)
    nums = text[:(nrows-1)]
    ops_row = text[nrows-1]

    starts = [i for i, val in enumerate(ops_row) if val != " "]
    n_problems = len(starts)
    ops = [ops_row[i] for i in starts]

    ncols = len(nums[1])
    ends = [pos - 1 for pos in starts[1:]]
    ends += [ncols]

    v_nums = ["" for _ in range(ncols)]
    for col in range(ncols):
        for row in range(nrows-1):
            v_nums[col] += nums[row][col]

    groups = [v_nums[start:end] for start, end in zip(starts, ends)]

    ans = 0
    for i in range(n_problems):
        group = [int(num) for num in groups[i]]
        if ops[i] == "+":
            ans += sum(group)
        else:
            ans += prod(group)

    return ans
