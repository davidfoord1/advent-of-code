def solve_day1_part1(text):
  """
  Compare the differences between 2 lists
  
  Parse the input text as 2 lists of numbers and sort 
  them. Then take the sum of the absolute differences between pairs.
  
  I imagine it's not too 'pythonic' even for such a simple puzzle. In porting
  my R solution with the limited Python that I know, lapply has become list 
  comprehensions and I've used a for loop to iterate over the 2 lists instead
  of a vectorised comparison. Perhaps NumPy could help out there?

  Parameters
  ----------
  text : list of str
      Where each string contains a number in the first list and a the second 
      list.

  Returns
  -------
  int
      The sum of absolute differences between the sorted lists
  """
  # extract to 2 lists of integers for ease of operations
  lines = [line.split() for line in text]
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
