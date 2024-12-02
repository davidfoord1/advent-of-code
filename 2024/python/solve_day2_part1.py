import numpy as np

def solve_day2_part1(input):
  reports_list = [line.split() for line in input]
  reports_list = [np.array(report, dtype = np.int32) for report in reports_list]
    
  diffs_list = [np.diff(report) for report in reports_list]
  
  return(sum([is_safe(diffs) for diffs in diffs_list]))
  
def is_safe(diffs):
  one_direction = all(diffs < 0) or all(diffs > 0)
  safe_size = all(abs(diffs) <= 3)
  
  return(one_direction and safe_size)
