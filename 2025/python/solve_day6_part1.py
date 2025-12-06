from math import prod

def solve_day6_part1(text):
    text = [line.rstrip().split() for line in text]

    nrows = len(text)
    nums = text[:(nrows-1)]
    ops = text[nrows-1]

    ans = 0
    n_problems = len(nums[0])

    for i in range(n_problems):
        group = []
        for row in range(nrows-1):
            group += [int(nums[row][i])]

        if ops[i] == "+":
            ans += sum(group)
        else:
            ans += prod(group)

    return(ans)
