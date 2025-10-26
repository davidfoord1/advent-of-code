def solve_day4_part1(text):
  xmas_count = 0
  
  for line_no in range(len(text)):
    for letter_no in range(len(text[line_no])):
      if text[line_no][letter_no] == "X":
        xmas_count += find_xmas_from_x(text, line_no, letter_no)
        
  print(xmas_count)
        
def find_xmas_from_x(text, line_no, letter_no):
  xmas_count = 0
  
  # list of directions
  # [letter_no, line_no]
  dir_list = [
    [-1, -1], # LU
    [0, -1], # U
    [1, -1], # RU
    
    [-1, 0], # L
    [1, 0], # R
    
    [-1, 1], # LD
    [0, 1], # D
    [1, 1] # RD
  ]
  
  letters_to_find = ["M", "A", "S"]
  
  line_max_index = len(text) - 1
  letter_max_index = len(text[1]) - 1
  
  letter_index = 0
  line_index = 1
  
  for search_dir in dir_list:
    search_letter = letter_no
    search_line = line_no
    
    for index in range(len(letters_to_find)):
      search_letter += search_dir[letter_index]
      search_line += search_dir[line_index]
     
      # skip out of bounds 
      if search_line < 0 or search_line > line_max_index:
        break
      if search_letter < 0 or search_letter > letter_max_index:
        break
      
      # word not found
      if text[search_line][search_letter] != letters_to_find[index]:
        break
      
      # word found
      if letters_to_find[index] == letters_to_find[len(letters_to_find) - 1]:
        xmas_count += 1
          
  return(xmas_count)
