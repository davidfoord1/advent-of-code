def solve_day4_part2(text):
  x_mas_count = 0
  
  for line_no in range(len(text)):
    for letter_no in range(len(text[line_no])):
      if text[line_no][letter_no] == "A":
        if is_cross_mass(text, line_no, letter_no):
          x_mas_count += 1
        
  print(x_mas_count)
  
def is_cross_mass(text, line_no, letter_no):
  letters_to_find = ["M", "S"]
  
  letter_index = 0
  line_index = 1
  
  line_max_index = len(text) - 1
  letter_max_index = len(text[1]) - 1
  
  diag_1 = [
    [-1, -1], # left up
    [1, 1] # right down
  ]
  
  found_letters = []
  
  for search_dir in diag_1:
    search_letter = letter_no + search_dir[letter_index]
    search_line = line_no + search_dir[line_index]
    
    # skip out of bounds 
    if search_line < 0 or search_line > line_max_index:
      break
    if search_letter < 0 or search_letter > letter_max_index:
      break
      
    found_letters.append(text[search_line][search_letter])
   
  found_letters.sort()
  
  print(found_letters)
    
  if not found_letters == letters_to_find:
    print("---")
    return(False)
  
  diag_2 = [
    [1, -1], # right up
    [-1, 1] # left down
  ]
  
  found_letters = []
  
  for search_dir in diag_2:
    search_letter = letter_no + search_dir[letter_index]
    search_line = line_no + search_dir[line_index]
    
    # skip out of bounds 
    if search_line < 0 or search_line > line_max_index:
      break
    if search_letter < 0 or search_letter > letter_max_index:
      break
    
    found_letters.append(text[search_line][search_letter])
    
  found_letters.sort()
  print(found_letters)
  
  if not found_letters == letters_to_find:
    return(False)
  
  return(True)
  
