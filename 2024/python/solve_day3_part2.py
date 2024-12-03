import re

def solve_day3_part2(text):
  """
  Sum of enabled multiplications
  
  Add do() to the start of the input. Uses regex to split the input on do() and
  don't(), removing all the sections that start with don't(). Then find the sum
  of multiplying remaining pairs.
  
  Parameters
  ----------
  text : list of str
    Lines to parse for numbers to multiply.
      
  Returns
  -------
  int
    The sum of results of enabled multiplications
  """
  line = "".join(text)
  line = "do()" + line
  
  splits = re.split("(?=do\(\))|(?=don't\(\))", line)
  splits = [line for line in splits if line[0:5] != "don't"]
  
  mul_sums = [sum_muls(line) for line in splits]
  
  return(sum(mul_sums))
  
def sum_muls(section):
  """
  Sum of multiplications
  
  Uses regex to find numbers in format mul(x, y). Multiplies each pair and
  adds the results together.
  
  Parameters
  ----------
  section : list of str
    Lines to parse for numbers to multiply.
      

  Returns
  -------
  int
    The sum of results of multiplications
  """
  muls = re.findall("(?<=mul\()[0-9]+,[0-9]+(?=\))", section)
  
  if len(muls) == 0:
    return(0)
  
  muls = [pair.split(",") for pair in muls]
  muls = [[int(num) for num in pair] for pair in muls]
  muls = [pair[0] * pair[1] for pair in muls]
  
  return(sum(muls))
