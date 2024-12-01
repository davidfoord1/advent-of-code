def solve_day1_part1(input):
  # extract to 2 lists of integers for ease of operations
  lines = [line.split() for line in input]
  first_list = [int(line[0]) for line in lines]
  second_list = [int(line[1]) for line in lines]
  
  # sort so that we pair up the smallest number in the left list with the
  # smallest number in the right list etc.
  first_list.sort()
  second_list.sort()
  
  distances = [None] * len(first_list)
  
  # we want the absolute distance
  # it doesn't matter which list has the larger number
  for i in range(len(first_list)):
    distances[i] = abs(second_list[i] - first_list[i])
  
  return(sum(distances))
  
  
