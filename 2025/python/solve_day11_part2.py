def solve_day11_part2(text):
    keys = [line[:3] for line in text]
    outs = [line[4:].split() for line in text]
    devices = dict(zip(keys, outs))

    svr = search("svr", "fft", devices)
    fft = search("fft", "dac", devices)
    dac = search("dac", "out", devices)

    return(svr * fft * dac)

def search(first, last, devices):
    curr_layer = {first:1}
    last_count = 0

    while len(curr_layer) > 0:
        next_layer = {}

        for dev in curr_layer:
            curr_count = curr_layer[dev]
            if dev == last:
                last_count += curr_count
                continue

            if dev == "out":
                continue

            next_devs = devices[dev]

            for next_dev in next_devs:
                if next_dev in next_layer:
                    next_layer[next_dev] += curr_count
                else:
                    next_layer[next_dev] = curr_count

        curr_layer = next_layer


    return(last_count)
