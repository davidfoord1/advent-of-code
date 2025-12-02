def solve_day5_part2(input):
  input = [line.replace("\n", "") for line in input]
  
  split_at = [i for i,v in enumerate(input) if v == ""]
  split_at = int(split_at[0])
  
  rules = input[0:split_at]
  pages = input[split_at+1:len(input)]
  
  # build page ordering rule lookup
  rules = [rule.split("|") for rule in rules]
  order_lookup = dict()
  
  for rule in rules:
    key = rule[0]
    if key in order_lookup:
      order_lookup[key].append(rule[1])
    else:
      order_lookup[key] = [(rule[1])]
      
  # iterate over page sequences checking whether any are out of order
  pages = [line.split(",") for line in pages]
  
  pages_are_in_order = [are_pages_in_order(sequence, order_lookup) for sequence in pages]
  
  pages_not_in_order = [v for i, v in enumerate(pages) if not pages_are_in_order[i]]
  middle_pages = [get_middle_number(seq, order_lookup) for seq in pages_not_in_order]
  middle_numbers = [int(page) for page in middle_pages]
  
  print(sum(middle_numbers))

def are_pages_in_order(sequence, order_lookup):
  for i in reversed(range(len(sequence))):
    if i == 0:
      break
    
    key = sequence[i]
    
    if key in order_lookup:
      lookup = order_lookup[key]
    else:
      continue
    
    for page_number in sequence[:i]:
      if page_number in lookup:
        return(False)
    
  return(True)

def get_middle_number(sequence, order_lookup):
  middle_loc = (len(sequence)-1)//2
  
  for i, v in enumerate(sequence):
    later_numbers = [page for page in sequence if page in order_lookup[v]]
    if len(later_numbers) == middle_loc:
      return(v)
    
  print("oops")
  
