import numpy as np

def solve_day2_part2(text):
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
    
  
  return(sum([is_safe(report) for report in reports_list]))

def is_safe(report):
  """
  Check if a report is safe with a dampener
  
  If a report is not ordinarily safe, try removing each single level to
  then check if is safe.
  
  I opted for a simpler approach than my R version - just trying removing every
  individual level in the report instead of checking which levels were unsafe. 
  Preferred to have this simpler logic as working with Python lists and numpy 
  arrays are new to me.
  
  Parameters
  ----------
  report : numpy array of int
      Where each int is a level in the report

  Returns
  -------
  bool
      Whether the differences are safe
  """
  if (is_safe_no_dampener(report)):
    return(True)
  
  for i in range(len(report)):
    new_report = np.delete(report, i)
    
    if (is_safe_no_dampener(new_report)):
      return(True)
    
  return(False)
    

def is_safe_no_dampener(report):
  """
  Check if a report is safe
  
  Check the report is safe by finding the differences and confirming:
     either all decreasing OR all increasing
     AND all absolute differences are within 1 to 3
  
  Parameters
  ----------
  report : numpy array of int
      Where each int is a level in the report

  Returns
  -------
  bool
      Whether the differences are safe
  """
  diffs = np.diff(report)
  
  one_direction = all(diffs < 0) or all(diffs > 0)
  safe_size = all(abs(diffs) <= 3)
  
  return(one_direction and safe_size)
