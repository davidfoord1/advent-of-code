import re

def solve_day3_part1(text):
  """
  Sum of multiplications
  
  Uses regex to find numbers in format mul(x, y). Multiplies each pair and
  adds the results together.
  
  Parameters
  ----------
  text : list of str
    Lines to parse for numbers to multiply.
      
  Returns
  -------
  int
    The sum of results of multiplications
  """
  muls = [re.findall("(?<=mul\()[0-9]+,[0-9]+(?=\))", line) for line in text]
  muls = [pair for line in muls for pair in line] 
  muls = [pair.split(",") for pair in muls]
  muls = [[int(num) for num in pair] for pair in muls]
  muls = [pair[0] * pair[1] for pair in muls]
  
  return(sum(muls))
