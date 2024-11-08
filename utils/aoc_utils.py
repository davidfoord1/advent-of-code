import time
import tracemalloc
  
def aoc_run(expr):
    tracemalloc.start()
    
    start_time = time.time()
    
    # Run the function
    result = eval(expr)
    
    end_time = time.time()
    
    current, peak = tracemalloc.get_traced_memory()
    
    tracemalloc.stop()
    
    elapsed = end_time - start_time
    
    if elapsed < 0.001:
      elapsed = "< 0.001"
    else:
      elapsed = round(elapsed, 3)
      
    mem_used = peak / 1024
    
    if mem_used < 1:
      mem_used = "< 1"
    else:
      mem_used = round(mem_used)
    
    print("Elapsed:", elapsed, "seconds")
    print("Memory: ", mem_used, "KB")
    
    return elapsed, mem_used

def aoc_read(day):
  day = str(day)
  
  with open("input/day" + day + ".txt") as file:
    return file.readlines()
  
def aoc_source(day, part):
  day = str(day)
  part = str(part)
  
  with open("python/solve_day" + day + "_part" + part + ".py") as f:
    exec(f.read(), globals())
