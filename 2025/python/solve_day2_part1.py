def solve_day1_part1(text):
  ranges = text[0].split(",")
  ranges = [str.split("-") for str in ranges]

  invalid_total = 0

  for nums in ranges:
    for i in range(int(nums[0]), int(nums[1])+1):
      i = str(i)
      nchars = len(i)

      if (nchars % 2 != 0):
        continue

      split = nchars // 2

      if i[:split] == i[split:]:
        invalid_total += int(i)

  return(invalid_total)
