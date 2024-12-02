def solve_day1_part2(text):
  """
  Score the similarities between 2 lists
  
  Parse the input text as 2 lists of numbers. Count the number of occurrences 
  of each item in the first list in the second list. Multiply each number by
  its count to get the similarity score. Finally sum the score.

  The list count method made this one fairly easy. Again I imagine there is a
  slightly slicker way then using a for loop, but not bad to have the basics!
  
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
  
  counts = [second_list.count(num) for num in first_list]
  
  similarity_scores = [None] * len(first_list)
  
  for i in range(len(first_list)):
    similarity_scores[i] = first_list[i] * counts[i]
  
  return(sum(similarity_scores))
