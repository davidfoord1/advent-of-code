def solve_day3_part2(text):
    banks = [list(line.strip("\n")) for line in text]
    joltage = [max_jolt(bank) for bank in banks]

    return(sum(joltage))

def max_jolt(bank, n = 12):
    digits = [None] * n
    prev_loc = -1
    bank_len = len(bank)

    for i in range(n):
        max_loc = bank_len - (n - i) + 1

        search_bank = bank[(prev_loc + 1):max_loc]
        digits[i] = max(search_bank)

        loc = search_bank.index(digits[i])
        prev_loc = prev_loc + loc + 1

    result = int(''.join(digits))

    return(result)
