def solve_day11_part1(text):
    first = "you"
    last = "out"

    keys = [line[:3] for line in text]
    outs = [line[4:].split() for line in text]
    devices = dict(zip(keys, outs))

    return(search(first, last, devices))

def search(first, last, devices):
    if first == last:
        return(1)

    next_devices = devices[first]
    paths = [search(next, last, devices) for next in next_devices]

    return(sum(paths))
