def solve_day2_part2(text):
  ranges = text[0].split(",")
  ranges = [str.split("-") for str in ranges]

  invalid_total = 0

  for nums in ranges:
    for num in range(int(nums[0]), int(nums[1])+1):
      if invalid(num):
        invalid_total += num

  return(invalid_total)

def invalid(num):
    num = str(num)
    nchars = len(num)

    for str_len in range(1, len(num)//2 + 1):
        invalid = True
        if (nchars - str_len) < 0 :
            break

        if (nchars % str_len) != 0:
            continue

        substr_chars = num[:str_len]
        count_to_check = (nchars - str_len) // str_len + 1

        for multi in range(1, count_to_check):
            step = multi * str_len
            if num[step:step+str_len] != substr_chars:
                invalid = False
                break

        if invalid:
            return(True)

    return(False)
