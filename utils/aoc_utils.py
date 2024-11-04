import time
  
def aoc_run(expr):
    start_time = time.time()
    
    result = eval(expr)
    
    elapsed = round(time.time() - start_time, 2)
    
    print("Answer:  ", result)
    print("Elapsed: ", elapsed, " seconds")
    
    return result, elapsed

def aoc_read(day):
  day = str(day)
  
  with open("input/day" + day + ".txt") as file:
    return file.readlines()
  
def aoc_source(day, part):
  day = str(day)
  part = str(part)
  
  with open("python/solve_day" + day + "_part" + part + ".py") as f:
    exec(f.read(), globals())
