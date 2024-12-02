import numpy as np

def solve_day2_part1(text):
  """
  Find how many reports are safe
  
  Check each report is
     either all decreasing OR all increasing
     AND all absolute differences are within 1 to 3
  
  Parameters
  ----------
  text : list of str
      Where each string is a report and each digit is a 'level'

  Returns
  -------
  int
    The number of safe reports
  
  """
  reports_list = [line.split() for line in text]
  reports_list = [np.array(report, dtype = np.int32) for report in reports_list]
    
  diffs_list = [np.diff(report) for report in reports_list]
  
  return(sum([is_safe(diffs) for diffs in diffs_list]))
  
def is_safe(diffs):
  """
  Check if the differences are safe
  
  Check the differences for
     either all decreasing OR all increasing
     AND all absolute differences are within 1 to 3
  
  Parameters
  ----------
  diffs : numpy array of int
      Where each int is a difference between consecutive levels in a report

  Returns
  -------
  bool
      Whether the differences are safe
  """
  one_direction = all(diffs < 0) or all(diffs > 0)
  safe_size = all(abs(diffs) <= 3)
  
  return(one_direction and safe_size)
